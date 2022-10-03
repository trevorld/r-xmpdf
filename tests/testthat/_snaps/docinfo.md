# conversion to/from docinfo()

    Code
      print(d)
    Output
      Author: John Doe
      CreationDate: 2022-11-11 11:11:11+0000
      Creator: Generic Creator
      Producer: Generic Producer
      Title: Generic Title
      Subject: Generic Subject
      Keywords: Key
      Keywords: Word
      ModDate: 2022-11-11 11:11:11+0000

---

    Code
      print(x)
    Output
      dc:Title : Generic Title
      dc:Creator : John Doe
      dc:Description : Generic Subject
      pdf:Producer : Generic Producer
      pdf:Keywords : Key, Word
      xmp:CreateDate : 2022-11-11 11:11:11+0000
      xmp:CreatorTool : Generic Creator
      xmp:ModifyDate : 2022-11-11 11:11:11+0000

