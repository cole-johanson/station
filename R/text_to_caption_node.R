text_to_caption_node <- function() {
  str_c(
    '<w:p w14:paraId=\"69A81C56\" w14:textId=\"65B9BC5C\">',
      '<w:pPr>',
        '<w:pStyle w:val=\"Caption\"/>',
      '</w:pPr>',
      '<w:r>',
      '<w:t xml:space=\"preserve\">Table </w:t>',
      '</w:r>',
      '<w:fldSimple w:instr=\" SEQ Table \\* ARABIC \">',
        '<w:r>',
          '<w:rPr>',
            '<w:noProof/>',
          '</w:rPr>',
          '<w:t>8</w:t>',
        '</w:r>',
      '</w:fldSimple>',
      '<w:r>',
        '<w:t>:</w:t>',
      '</w:r>',
      '<w:r>',
        '<w:tab/>',
        '<w:t xml:space=\"preserve\">Patient Summarized by Analysis Set (Full Analysis Set)</w:t>',
      '</w:r>',
    '</w:p>'
  )
}
