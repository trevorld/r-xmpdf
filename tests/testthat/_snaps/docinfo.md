# conversion to/from docinfo()

    Code
      print(d)
    Output
      Author: John Doe
      CreationDate: 2022-11-11T11:11:11
      Creator: Generic Creator
      Producer: Generic Producer
      Title: Generic Title
      Subject: Generic Subject
      Keywords: Key, Word
      ModDate: 2022-11-11T11:11:11

---

    Code
      print(x)
    Output
      auto_xmp (not XMP tag):  photoshop:Credit
      dc:creator := John Doe
      dc:description := Generic Subject
      dc:title := Generic Title
      pdf:Keywords := Key, Word
      pdf:Producer := Generic Producer
      => photoshop:Credit = John Doe
      xmp:CreateDate := 2022-11-11T11:11:11
      xmp:CreatorTool := Generic Creator
      xmp:ModifyDate := 2022-11-11T11:11:11

