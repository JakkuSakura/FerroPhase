use fp_core::vfs::{InMemoryFileSystem, VirtualFileSystem, VirtualPath};

#[test]
fn in_memory_fs_round_trip() {
    let fs = InMemoryFileSystem::new();
    let file_path = VirtualPath::new_absolute(["tmp", "example.txt"]);
    let contents = b"hello world";

    fs.write(&file_path, contents).expect("write succeeds");

    let metadata = fs.metadata(&file_path).expect("metadata");
    assert_eq!(metadata.size, contents.len() as u64);
    assert_eq!(metadata.kind, fp_core::vfs::FileKind::File);

    let round_trip = fs.read(&file_path).expect("read");
    assert_eq!(round_trip, contents);

    let dir_entries = fs
        .read_dir(&VirtualPath::new_absolute(["tmp"]))
        .expect("list directory");
    assert_eq!(dir_entries.len(), 1);
    assert_eq!(dir_entries[0].path, file_path);

    fs.remove(&file_path).expect("remove");
    assert!(!fs.exists(&file_path));
}
