use crate::classfile::EmittedClass;
use crate::error::JvmError;

const LOCAL_FILE_HEADER_SIGNATURE: u32 = 0x0403_4B50;
const CENTRAL_DIRECTORY_HEADER_SIGNATURE: u32 = 0x0201_4B50;
const END_OF_CENTRAL_DIRECTORY_SIGNATURE: u32 = 0x0605_4B50;
const STORED_METHOD: u16 = 0;

#[derive(Debug, Clone, PartialEq, Eq)]
struct JarEntry {
    name: String,
    bytes: Vec<u8>,
    crc32: u32,
    offset: u32,
}

pub fn emit_executable_jar(
    classes: &[EmittedClass],
    main_class: &str,
) -> Result<Vec<u8>, JvmError> {
    let mut entries = Vec::with_capacity(classes.len() + 1);
    entries.push(build_entry(
        "META-INF/MANIFEST.MF".to_string(),
        manifest_contents(main_class).into_bytes(),
    )?);
    for class in classes {
        entries.push(build_entry(
            format!("{}.class", class.internal_name),
            class.bytes.clone(),
        )?);
    }

    let mut out = Vec::new();
    for entry in &mut entries {
        entry.offset = out.len() as u32;
        write_local_file_header(&mut out, entry);
        out.extend_from_slice(entry.name.as_bytes());
        out.extend_from_slice(&entry.bytes);
    }

    let central_directory_offset = out.len() as u32;
    for entry in &entries {
        write_central_directory_header(&mut out, entry);
        out.extend_from_slice(entry.name.as_bytes());
    }
    let central_directory_size = out.len() as u32 - central_directory_offset;

    write_u32(&mut out, END_OF_CENTRAL_DIRECTORY_SIGNATURE);
    write_u16(&mut out, 0);
    write_u16(&mut out, 0);
    write_u16(&mut out, entries.len() as u16);
    write_u16(&mut out, entries.len() as u16);
    write_u32(&mut out, central_directory_size);
    write_u32(&mut out, central_directory_offset);
    write_u16(&mut out, 0);

    Ok(out)
}

fn manifest_contents(main_class: &str) -> String {
    format!(
        "Manifest-Version: 1.0\r\nCreated-By: FerroPhase\r\nMain-Class: {}\r\n\r\n",
        main_class.replace('/', ".")
    )
}

fn build_entry(name: String, bytes: Vec<u8>) -> Result<JarEntry, JvmError> {
    let _size = u32::try_from(bytes.len())
        .map_err(|_| JvmError::Lowering(format!("jar entry too large: {name}")))?;
    Ok(JarEntry {
        name,
        crc32: crc32fast::hash(&bytes),
        bytes,
        offset: 0,
    })
}

fn write_local_file_header(out: &mut Vec<u8>, entry: &JarEntry) {
    write_u32(out, LOCAL_FILE_HEADER_SIGNATURE);
    write_u16(out, 20);
    write_u16(out, 0);
    write_u16(out, STORED_METHOD);
    write_u16(out, 0);
    write_u16(out, 0);
    write_u32(out, entry.crc32);
    write_u32(out, entry.bytes.len() as u32);
    write_u32(out, entry.bytes.len() as u32);
    write_u16(out, entry.name.len() as u16);
    write_u16(out, 0);
}

fn write_central_directory_header(out: &mut Vec<u8>, entry: &JarEntry) {
    write_u32(out, CENTRAL_DIRECTORY_HEADER_SIGNATURE);
    write_u16(out, 20);
    write_u16(out, 20);
    write_u16(out, 0);
    write_u16(out, STORED_METHOD);
    write_u16(out, 0);
    write_u16(out, 0);
    write_u32(out, entry.crc32);
    write_u32(out, entry.bytes.len() as u32);
    write_u32(out, entry.bytes.len() as u32);
    write_u16(out, entry.name.len() as u16);
    write_u16(out, 0);
    write_u16(out, 0);
    write_u16(out, 0);
    write_u16(out, 0);
    write_u32(out, 0);
    write_u32(out, entry.offset);
}

fn write_u16(out: &mut Vec<u8>, value: u16) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn write_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_le_bytes());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emits_jar_with_manifest_and_class() {
        let jar = emit_executable_jar(
            &[EmittedClass {
                internal_name: "demo_app".to_string(),
                bytes: vec![0xCA, 0xFE, 0xBA, 0xBE],
            }],
            "demo_app",
        )
        .expect("jar emission should succeed");

        assert_eq!(&jar[0..4], b"PK\x03\x04");
        assert!(
            jar.windows("META-INF/MANIFEST.MF".len())
                .any(|window| window == b"META-INF/MANIFEST.MF")
        );
        assert!(
            jar.windows("Main-Class: demo_app".len())
                .any(|window| window == b"Main-Class: demo_app")
        );
        assert!(
            jar.windows("demo_app.class".len())
                .any(|window| window == b"demo_app.class")
        );
    }
}
