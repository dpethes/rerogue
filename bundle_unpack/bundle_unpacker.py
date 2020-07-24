import struct
import argparse
import os

debug = True
padding = 65570
file_entry_size = 252
crypt_key = 170 # 0xAA

def check_expected( value, expected ):
    assert value == expected, f"Invalid format: expected {repr(expected)}, got {repr(value)}"

def do_unpack(input_file_path, output_dir_path):
    with open(input_file_path, "rb") as f:
        header = f.read(4)
        expected = b"BUND"
        check_expected(header, b"BUND")
        # big endian
        archive_size = struct.unpack(">I", f.read(4))[0]
        print(f"Got archive size {archive_size} bytes")
        bundle_header = f.read(4)
        expected = b"BNHD"
        check_expected(bundle_header, b"BNHD")
        # big endian
        bundle_header_size = struct.unpack(">I", f.read(4))[0]
        print(f"Got bundle header size {bundle_header_size} bytes")
        f.seek(padding, 1)
        fe = 0
        file_entries = []
        remainder = (bundle_header_size - padding) % file_entry_size
        assert remainder == 0, "Invalid format: bundle header size (minus padding) is not a multiple of the expected file entry size!"
        
        num_file_entries = (bundle_header_size - padding)//file_entry_size
        remainder = (bundle_header_size - padding) % file_entry_size
        print(f"Calculated {num_file_entries} entries")
        while fe < num_file_entries:
            filename_encrypted = f.read(200)
            filename_decrypted = bytearray()
            for e in filename_encrypted:
                d = e ^ crypt_key
                if d == 0:
                    break
                filename_decrypted.append(d)
            # little endian
            filedata_length = struct.unpack("<I", f.read(4))[0]
            filedata_offset = struct.unpack("<I", f.read(4))[0]
            f.seek(44,1)
            file_entries.append({"filename": filename_decrypted.decode("ascii"),
                                 "length" : filedata_length,
                                 "offset" : filedata_offset})
            print(f"Got file {filename_decrypted.decode('ascii')}, length {filedata_length}, offset {filedata_offset} (bytes)")
            fe += 1
        file_data_header = f.read(4)
        check_expected(file_data_header, b"BNDT")
        # big endian
        file_data_size = struct.unpack(">I", f.read(4))[0]
        print(f"Got total file data size {file_data_size} bytes")
        base_pos = f.tell()
        for fe in file_entries:
            start_pos = base_pos + fe['offset']
            f.seek(start_pos)
            file_data = f.read(fe["length"])
            full_filename = os.path.join(output_dir_path, fe['filename'][1:])
            print(f"{full_filename} (filepos {start_pos}, length {fe['length']})")
            os.makedirs(os.path.dirname(full_filename), exist_ok=True)
            with open(full_filename, "wb+") as fo:
                fo.write(file_data)
            print(f"Wrote {len(file_data)} bytes")

def do_pack(input_dir_path, output_file_path):
    all_file_pairs = []
    for dirpath, dirnames, files in os.walk(input_dir_path, topdown=False):
        for file in files:
            unpacked_full_filename = os.path.join(dirpath,file)
            rel_full_filename = os.path.relpath(unpacked_full_filename, input_dir_path)
            packed_filename = os.path.join(os.path.sep,rel_full_filename)
            all_file_pairs.append((unpacked_full_filename, packed_filename))

    all_file_data = {}
    offset = 0
    for unpacked_fn, packed_fn in all_file_pairs:
        with open(unpacked_fn, "rb") as f:
            file_data = f.read()
            file_size = len(file_data)
        all_file_data[(unpacked_fn, packed_fn)] = {"size": file_size, "offset": offset, "file_data": file_data}
        offset += file_size
    N = len(all_file_pairs)
    bundle_header_size = padding + N * file_entry_size
    total_archive_size = 4*4 + bundle_header_size + 2*4 + offset
    
    with open(output_file_path, "wb+") as f:
        f.write(b"BUND")
        f.write(struct.pack(">I", total_archive_size - 2*4)) # big endian
        f.write(b"BNHD")
        f.write(struct.pack(">I", bundle_header_size)) # big endian
        f.write(bytearray(padding))
        file_data = bytearray()
        for unpacked_fn, packed_fn in all_file_pairs:
            decrypted_fn = packed_fn.encode("ascii")
            if len(decrypted_fn) > 199:
                print("Warning, truncating packed filename {packed_fn} to 199 characters (plus null terminator)")
                decrypted_fn = decrypted_fn[:199]
            encrypted_fn = bytearray(200)
            for i, b in enumerate(decrypted_fn):
                e = b ^ crypt_key
                encrypted_fn[i] = e
            encrypted_fn[i+1] = crypt_key # null terminator (0 ^ crypt_key = crypt_key)
            f.write(encrypted_fn)
            file_info_dict = all_file_data[(unpacked_fn, packed_fn)]
            # little endian
            f.write(struct.pack("<I", file_info_dict["size"]))
            f.write(struct.pack("<I", file_info_dict["offset"]))
            f.write(bytearray(44))
            file_data.extend(bytearray(file_info_dict["file_data"]))
        f.write(b"BNDT")
        f.write(struct.pack(">I", len(file_data))) # big endian
        f.write(file_data)          
        
def main():
    parser = argparse.ArgumentParser(description="Unpack Rogue Squadron BUNDLE files")
    parser.add_argument("--input-path", metavar="PATH", type=str)
    parser.add_argument("--output-path", metavar="PATH", type=str)
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--unpack", action="store_true")
    group.add_argument("--pack", action="store_true")
    args = parser.parse_args()
    if args.pack:
        raise NotImplementedError("Currently unable to produce compatible packed BUNDLE.00x files -- more research is needed. "
                                  "See do_pack in the source code.")
        #do_pack(args.input_path, args.output_path)
    else:
        do_unpack(args.input_path, args.output_path)

if __name__ == "__main__":
    main()






