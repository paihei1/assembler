use super::{LabelContext};

pub fn encode(mnemonic: &[u8], _operands: &[u8], _labels: &mut LabelContext) -> Result<Option<(usize, [u8;16])>, String> {
    match mnemonic {
        // TODO
        _ => Ok(None)
    }
}