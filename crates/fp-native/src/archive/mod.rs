use fp_core::error::{Error, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArchiveMember {
    pub name: String,
    pub data: Vec<u8>,
}

pub fn can_read_archive(bytes: &[u8]) -> bool {
    object::read::archive::ArchiveFile::parse(bytes).is_ok()
}

pub fn read_archive_members(bytes: &[u8]) -> Result<Vec<ArchiveMember>> {
    let archive = object::read::archive::ArchiveFile::parse(bytes)
        .map_err(|err| Error::from(format!("Failed to parse archive: {err}")))?;

    let mut members = Vec::new();
    for member in archive.members() {
        let member =
            member.map_err(|err| Error::from(format!("Failed to read archive member: {err}")))?;
        let name = String::from_utf8_lossy(member.name()).to_string();
        let data = member
            .data(bytes)
            .map_err(|err| Error::from(format!("Failed to read archive member data: {err}")))?;
        members.push(ArchiveMember {
            name,
            data: data.to_vec(),
        });
    }
    Ok(members)
}

pub fn write_gnu_archive(members: &[ArchiveMember]) -> Result<Vec<u8>> {
    let mut out = Vec::new();
    out.extend_from_slice(b"!<arch>\n");

    let mut names_table = Vec::new();
    let mut long_name_offsets = std::collections::HashMap::new();
    for member in members {
        let name_bytes = member.name.as_bytes();
        if name_bytes.len() > 15 {
            let offset = names_table.len();
            long_name_offsets.insert(member.name.clone(), offset);
            names_table.extend_from_slice(name_bytes);
            names_table.extend_from_slice(b"/\n");
        }
    }

    if !names_table.is_empty() {
        write_member_header(&mut out, b"//", names_table.len())?;
        out.extend_from_slice(&names_table);
        if (names_table.len() & 1) != 0 {
            out.push(b'\n');
        }
    }

    for member in members {
        let name_field = if member.name.as_bytes().len() > 15 {
            let offset = long_name_offsets
                .get(&member.name)
                .copied()
                .ok_or_else(|| Error::from("missing long-name table offset"))?;
            format!("/{offset}")
        } else {
            format!("{}/", member.name)
        };

        write_member_header(&mut out, name_field.as_bytes(), member.data.len())?;
        out.extend_from_slice(&member.data);
        if (member.data.len() & 1) != 0 {
            out.push(b'\n');
        }
    }

    Ok(out)
}

fn write_member_header(out: &mut Vec<u8>, name: &[u8], size: usize) -> Result<()> {
    if name.is_empty() {
        return Err(Error::from("archive member name is empty"));
    }

    let size = u64::try_from(size).map_err(|_| Error::from("archive member too large"))?;

    write_fixed_ascii(out, name, 16, b' ')?;
    write_fixed_decimal(out, 0, 12)?; // date
    write_fixed_decimal(out, 0, 6)?; // uid
    write_fixed_decimal(out, 0, 6)?; // gid
    write_fixed_octal(out, 0o644, 8)?; // mode
    write_fixed_decimal(out, size, 10)?; // size
    out.extend_from_slice(b"`\n");
    Ok(())
}

fn write_fixed_ascii(out: &mut Vec<u8>, bytes: &[u8], width: usize, pad: u8) -> Result<()> {
    if bytes.len() > width {
        return Err(Error::from("archive member name too long"));
    }
    out.extend_from_slice(bytes);
    out.resize(out.len() + (width - bytes.len()), pad);
    Ok(())
}

fn write_fixed_decimal(out: &mut Vec<u8>, value: u64, width: usize) -> Result<()> {
    let s = value.to_string();
    if s.len() > width {
        return Err(Error::from("archive numeric field overflow"));
    }
    out.extend_from_slice(s.as_bytes());
    out.resize(out.len() + (width - s.len()), b' ');
    Ok(())
}

fn write_fixed_octal(out: &mut Vec<u8>, value: u64, width: usize) -> Result<()> {
    let s = format!("{value:o}");
    if s.len() > width {
        return Err(Error::from("archive mode field overflow"));
    }
    out.extend_from_slice(s.as_bytes());
    out.resize(out.len() + (width - s.len()), b' ');
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gnu_archive_roundtrips_member_names_and_bytes() {
        let members = vec![
            ArchiveMember {
                name: "short.o".to_string(),
                data: vec![1, 2, 3],
            },
            ArchiveMember {
                name: "this_is_a_very_long_member_name.o".to_string(),
                data: vec![4, 5, 6, 7],
            },
        ];

        let bytes = write_gnu_archive(&members).expect("write archive");
        let parsed = read_archive_members(&bytes).expect("read archive");

        assert!(
            parsed
                .iter()
                .any(|member| member.name == "short.o" && member.data == vec![1, 2, 3])
        );
        assert!(parsed.iter().any(|member| {
            member.name == "this_is_a_very_long_member_name.o" && member.data == vec![4, 5, 6, 7]
        }));
    }
}
