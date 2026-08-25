use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EbpfHelperMetadata {
    pub id: u32,
    pub name: String,
    pub symbol: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EbpfFormatMetadata {
    pub id: u32,
    pub format: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EbpfCallsiteMetadata {
    pub function: String,
    pub offset: u32,
    pub helper_id: u32,
    pub helper_symbol: String,
    pub format_id: Option<u32>,
    pub arg_count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EbpfObjectMetadata {
    pub abi: String,
    pub helpers: Vec<EbpfHelperMetadata>,
    pub formats: Vec<EbpfFormatMetadata>,
    pub callsites: Vec<EbpfCallsiteMetadata>,
}

pub fn read_object_metadata(bytes: &[u8]) -> Result<EbpfObjectMetadata> {
    let file = ObjectFile::parse(bytes).map_err(|err| Error::from(err.to_string()))?;
    let abi = read_utf8_section(&file, ".fp.ebpf.abi")?.unwrap_or_default();
    let helpers = if let Some(data) = read_binary_section(&file, ".fp.ebpf.helpers")? {
        decode_helpers(data)?
    } else {
        Vec::new()
    };
    let formats = if let Some(data) = read_binary_section(&file, ".fp.ebpf.fmt")? {
        decode_formats(data)?
    } else {
        Vec::new()
    };
    let callsites = if let Some(data) = read_binary_section(&file, ".fp.ebpf.calls")? {
        decode_callsites(data)?
    } else {
        Vec::new()
    };

    Ok(EbpfObjectMetadata {
        abi,
        helpers,
        formats,
        callsites,
    })
}

fn read_utf8_section(file: &ObjectFile<'_>, name: &str) -> Result<Option<String>> {
    let Some(section) = file.section_by_name(name) else {
        return Ok(None);
    };
    let data = section.data().map_err(|err| Error::from(err.to_string()))?;
    let text = std::str::from_utf8(data).map_err(|err| Error::from(err.to_string()))?;
    Ok(Some(text.to_string()))
}

fn read_binary_section<'a>(file: &'a ObjectFile<'a>, name: &str) -> Result<Option<&'a [u8]>> {
    let Some(section) = file.section_by_name(name) else {
        return Ok(None);
    };
    section
        .data()
        .map(Some)
        .map_err(|err| Error::from(err.to_string()))
}

fn decode_helpers(mut data: &[u8]) -> Result<Vec<EbpfHelperMetadata>> {
    let count = read_u32(&mut data)? as usize;
    let mut helpers = Vec::with_capacity(count);
    for _ in 0..count {
        let id = read_u32(&mut data)?;
        let name = read_string(&mut data)?;
        let symbol = read_string(&mut data)?;
        helpers.push(EbpfHelperMetadata { id, name, symbol });
    }
    ensure_section_consumed(data, ".fp.ebpf.helpers")?;
    Ok(helpers)
}

fn decode_formats(mut data: &[u8]) -> Result<Vec<EbpfFormatMetadata>> {
    let mut formats = Vec::new();
    while !data.is_empty() {
        let id = read_u32(&mut data)?;
        let format = read_string(&mut data)?;
        formats.push(EbpfFormatMetadata { id, format });
    }
    Ok(formats)
}

fn decode_callsites(mut data: &[u8]) -> Result<Vec<EbpfCallsiteMetadata>> {
    let count = read_u32(&mut data)? as usize;
    let mut callsites = Vec::with_capacity(count);
    for _ in 0..count {
        let function = read_string(&mut data)?;
        let offset = read_u32(&mut data)?;
        let helper_id = read_u32(&mut data)?;
        let helper_symbol = read_string(&mut data)?;
        let format_id = match read_u32(&mut data)? {
            u32::MAX => None,
            value => Some(value),
        };
        let arg_count = read_u32(&mut data)?;
        callsites.push(EbpfCallsiteMetadata {
            function,
            offset,
            helper_id,
            helper_symbol,
            format_id,
            arg_count,
        });
    }
    ensure_section_consumed(data, ".fp.ebpf.calls")?;
    Ok(callsites)
}

fn read_u32(data: &mut &[u8]) -> Result<u32> {
    if data.len() < 4 {
        return Err(Error::from("truncated eBPF metadata section"));
    }
    let (prefix, rest) = data.split_at(4);
    *data = rest;
    Ok(u32::from_le_bytes(prefix.try_into().expect("u32 width")))
}

fn read_string(data: &mut &[u8]) -> Result<String> {
    let len = read_u32(data)? as usize;
    if data.len() < len {
        return Err(Error::from("truncated eBPF metadata string"));
    }
    let (prefix, rest) = data.split_at(len);
    *data = rest;
    std::str::from_utf8(prefix)
        .map(|value| value.to_string())
        .map_err(|err| Error::from(err.to_string()))
}

fn ensure_section_consumed(data: &[u8], section: &str) -> Result<()> {
    if data.is_empty() {
        Ok(())
    } else {
        Err(Error::from(format!(
            "unexpected trailing bytes in {} metadata section",
            section
        )))
    }
}
