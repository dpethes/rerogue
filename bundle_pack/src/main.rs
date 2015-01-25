#![allow(unstable)]
extern crate core;
extern crate collections;

use std::io::File;
use std::os;
use collections::string::String;


struct FileEntry {
    filename: String,
    length: u32,
    offset: u32
}

fn unpack_bundle(fpath: &Path) {
    // check file magic
    let mut f = File::open(fpath).unwrap();
    let mut magic = String::new();
    for i in 0..4 {
        let c:u8 = f.read_u8().unwrap();
        magic.push(c as char);
    }
    println!("Magic: {}", magic);

    // file data size (= filesize - 8)
    let asize = f.read_be_u32().unwrap();
    println!("Archive size: {}", asize);

    // bundle header
    let mut bheader = String::new();
    for i in 0..4 {
        let c:u8 = f.read_u8().unwrap();
        bheader.push(c as char);
    }
    let bsize = f.read_be_u32().unwrap();
    println!("Bundle header: {}  size: {}", bheader, bsize);

    // skip padding
    f.seek(65570, std::io::SeekStyle::SeekCur);

    // read file entries
    let file_entries = (bsize - 65570) / 252;
    println!("File entries: {}", file_entries);

    let mut files = Vec::new();
    for i in 0..file_entries {
        let mut fname = String::new();
        let input_fname = f.read_exact(200).unwrap();
        for byte in input_fname.iter() {
            let c = byte ^ 0xAA;
            if c == 0 {
                break;
            }
            fname.push(c as char);
        }
        let fsize = f.read_le_u32().unwrap();
        let foffset = f.read_le_u32().unwrap();
        println!("File name: {} offset: {} size: {}", fname, foffset, fsize);
        f.read_exact(44).unwrap();

        let file = FileEntry {filename: fname, length: fsize, offset: foffset};
        files.push(file);
    }

    // bundle data
    let mut bdata = String::new();
    for i in 0..4 {
        let c:u8 = f.read_u8().unwrap();
        bdata.push(c as char);
    }
    let bdata_size = f.read_be_u32().unwrap();
    println!("Bundle data: {} size: {}", bdata, bdata_size);

    // unpack files
    let base_pos = f.tell().unwrap();
    for file in files.iter() {
        let mut split = file.filename.split_str("\\");
        let vec: Vec<&str> = split.collect();
        let last  = vec[vec.len() - 1];
        println!("File name: {} offset: {} size: {}", last, file.offset, file.length);

        let fpos = base_pos + file.offset as u64;
        f.seek((fpos as i64), std::io::SeekStyle::SeekSet);
        let databytes = f.read_exact(file.length as usize).unwrap();
        let mut fdest = File::create(&Path::new(last));
        fdest.write(databytes.as_slice());
    }
}

fn main() {
    let args = os::args();
    if args.len() < 2 {
        println!("No input specified!");  //first param = path
        unpack_bundle(&Path::new("BUNDLE.000"));  //debug - remove
    } else {
        println!("Trying to parse file {0}", args[1]);
        let ref fpath = args[1];
        unpack_bundle(&Path::new(fpath));
    }
    print!("Done.");
}
