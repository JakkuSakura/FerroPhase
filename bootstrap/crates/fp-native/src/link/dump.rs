use fp_core::error::{Error, Result};

#[derive(Debug, Clone)]
pub struct MachoHeader {
    pub ncmds: u32,
    pub sizeofcmds: u32,
    pub flags: u32,
}

#[derive(Debug, Clone)]
pub struct MachoSegment {
    pub name: String,
    pub vmaddr: u64,
    pub vmsize: u64,
    pub fileoff: u64,
    pub filesize: u64,
    pub nsects: u32,
}

#[derive(Debug, Clone)]
pub struct MachoDyldInfo {
    pub rebase_off: u32,
    pub rebase_size: u32,
    pub bind_off: u32,
    pub bind_size: u32,
    pub lazy_bind_off: u32,
    pub lazy_bind_size: u32,
    pub export_off: u32,
    pub export_size: u32,
}

#[derive(Debug, Clone)]
pub struct MachoSymtab {
    pub symoff: u32,
    pub nsyms: u32,
    pub stroff: u32,
    pub strsize: u32,
}

#[derive(Debug, Clone)]
pub struct MachoDysymtab {
    pub ilocalsym: u32,
    pub nlocalsym: u32,
    pub iextdefsym: u32,
    pub nextdefsym: u32,
    pub iundefsym: u32,
    pub nundefsym: u32,
    pub indirectsymoff: u32,
    pub nindirectsyms: u32,
}

#[derive(Debug, Clone)]
pub struct MachoDump {
    pub header: MachoHeader,
    pub segments: Vec<MachoSegment>,
    pub dyld_info: Option<MachoDyldInfo>,
    pub symtab: Option<MachoSymtab>,
    pub dysymtab: Option<MachoDysymtab>,
    pub main_entryoff: Option<u64>,
}

impl MachoDump {
    pub fn to_pretty_string(&self) -> String {
        let mut out = String::new();
        out.push_str(&format!(
            "macho header: ncmds={}, sizeofcmds={}, flags=0x{:08x}\n",
            self.header.ncmds, self.header.sizeofcmds, self.header.flags
        ));
        for seg in &self.segments {
            out.push_str(&format!(
                "segment {} vmaddr=0x{:x} vmsize=0x{:x} fileoff={} filesize={} nsects={}\n",
                seg.name, seg.vmaddr, seg.vmsize, seg.fileoff, seg.filesize, seg.nsects
            ));
        }
        if let Some(info) = &self.dyld_info {
            out.push_str(&format!(
                "dyld rebase={}+{} bind={}+{} lazy_bind={}+{} export={}+{}\n",
                info.rebase_off,
                info.rebase_size,
                info.bind_off,
                info.bind_size,
                info.lazy_bind_off,
                info.lazy_bind_size,
                info.export_off,
                info.export_size
            ));
        }
        if let Some(symtab) = &self.symtab {
            out.push_str(&format!(
                "symtab symoff={} nsyms={} stroff={} strsize={}\n",
                symtab.symoff, symtab.nsyms, symtab.stroff, symtab.strsize
            ));
        }
        if let Some(dysymtab) = &self.dysymtab {
            out.push_str(&format!(
                "dysymtab ilocalsym={} nlocalsym={} iextdefsym={} nextdefsym={} iundefsym={} nundefsym={} indirectsymoff={} nindirectsyms={}\n",
                dysymtab.ilocalsym,
                dysymtab.nlocalsym,
                dysymtab.iextdefsym,
                dysymtab.nextdefsym,
                dysymtab.iundefsym,
                dysymtab.nundefsym,
                dysymtab.indirectsymoff,
                dysymtab.nindirectsyms
            ));
        }
        if let Some(entryoff) = self.main_entryoff {
            out.push_str(&format!("entryoff={}\n", entryoff));
        }
        out
    }
}

pub fn dump_macho(bytes: &[u8]) -> Result<MachoDump> {
    const MH_MAGIC_64: u32 = 0xfeed_facf;
    const LC_SEGMENT_64: u32 = 0x19;
    const LC_SYMTAB: u32 = 0x2;
    const LC_DYSYMTAB: u32 = 0xb;
    const LC_DYLD_INFO_ONLY: u32 = 0x8000_0022;
    const LC_MAIN: u32 = 0x8000_0028;

    let magic = read_u32(bytes, 0)?;
    if magic != MH_MAGIC_64 {
        return Err(Error::from("unsupported Mach-O magic"));
    }
    let ncmds = read_u32(bytes, 16)?;
    let sizeofcmds = read_u32(bytes, 20)?;
    let flags = read_u32(bytes, 24)?;
    let header = MachoHeader {
        ncmds,
        sizeofcmds,
        flags,
    };

    let mut segments = Vec::new();
    let mut dyld_info = None;
    let mut symtab = None;
    let mut dysymtab = None;
    let mut main_entryoff = None;

    let mut offset = 32usize;
    for _ in 0..ncmds {
        let cmd = read_u32(bytes, offset)?;
        let cmdsize = read_u32(bytes, offset + 4)? as usize;
        if cmdsize < 8 || offset + cmdsize > bytes.len() {
            return Err(Error::from("invalid Mach-O load command size"));
        }

        match cmd {
            LC_SEGMENT_64 => {
                let name = read_fixed_str(bytes, offset + 8, 16)?;
                let vmaddr = read_u64(bytes, offset + 24)?;
                let vmsize = read_u64(bytes, offset + 32)?;
                let fileoff = read_u64(bytes, offset + 40)?;
                let filesize = read_u64(bytes, offset + 48)?;
                let nsects = read_u32(bytes, offset + 64)?;
                segments.push(MachoSegment {
                    name,
                    vmaddr,
                    vmsize,
                    fileoff,
                    filesize,
                    nsects,
                });
            }
            LC_DYLD_INFO_ONLY => {
                dyld_info = Some(MachoDyldInfo {
                    rebase_off: read_u32(bytes, offset + 8)?,
                    rebase_size: read_u32(bytes, offset + 12)?,
                    bind_off: read_u32(bytes, offset + 16)?,
                    bind_size: read_u32(bytes, offset + 20)?,
                    lazy_bind_off: read_u32(bytes, offset + 24)?,
                    lazy_bind_size: read_u32(bytes, offset + 28)?,
                    export_off: read_u32(bytes, offset + 32)?,
                    export_size: read_u32(bytes, offset + 36)?,
                });
            }
            LC_SYMTAB => {
                symtab = Some(MachoSymtab {
                    symoff: read_u32(bytes, offset + 8)?,
                    nsyms: read_u32(bytes, offset + 12)?,
                    stroff: read_u32(bytes, offset + 16)?,
                    strsize: read_u32(bytes, offset + 20)?,
                });
            }
            LC_DYSYMTAB => {
                dysymtab = Some(MachoDysymtab {
                    ilocalsym: read_u32(bytes, offset + 8)?,
                    nlocalsym: read_u32(bytes, offset + 12)?,
                    iextdefsym: read_u32(bytes, offset + 16)?,
                    nextdefsym: read_u32(bytes, offset + 20)?,
                    iundefsym: read_u32(bytes, offset + 24)?,
                    nundefsym: read_u32(bytes, offset + 28)?,
                    indirectsymoff: read_u32(bytes, offset + 56)?,
                    nindirectsyms: read_u32(bytes, offset + 60)?,
                });
            }
            LC_MAIN => {
                main_entryoff = Some(read_u64(bytes, offset + 8)?);
            }
            _ => {}
        }

        offset += cmdsize;
    }

    Ok(MachoDump {
        header,
        segments,
        dyld_info,
        symtab,
        dysymtab,
        main_entryoff,
    })
}

fn read_u32(bytes: &[u8], offset: usize) -> Result<u32> {
    let end = offset + 4;
    if end > bytes.len() {
        return Err(Error::from("unexpected EOF while reading u32"));
    }
    Ok(u32::from_le_bytes(bytes[offset..end].try_into().unwrap()))
}

fn read_u64(bytes: &[u8], offset: usize) -> Result<u64> {
    let end = offset + 8;
    if end > bytes.len() {
        return Err(Error::from("unexpected EOF while reading u64"));
    }
    Ok(u64::from_le_bytes(bytes[offset..end].try_into().unwrap()))
}

fn read_fixed_str(bytes: &[u8], offset: usize, len: usize) -> Result<String> {
    let end = offset + len;
    if end > bytes.len() {
        return Err(Error::from("unexpected EOF while reading string"));
    }
    let raw = &bytes[offset..end];
    let s = std::str::from_utf8(raw).map_err(|_| Error::from("invalid UTF-8 in string"))?;
    Ok(s.trim_matches('\0').to_string())
}
