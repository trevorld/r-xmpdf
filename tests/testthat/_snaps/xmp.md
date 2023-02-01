# get_xmp() / set_xmp()

    Code
      print(xmp())
    Output
      No XMP metadata found

---

    Code
      print(x)
    Output
      spdx_id (not XMP tag) := CC-BY-4.0
      auto_xmp (not XMP tag):  photoshop:Credit
      dc:creator := A creator
      dc:description := A description
      dc:title := An XMP title
      pdf:Keywords := R, xmpdf
      pdf:Producer := R
      => photoshop:Credit = A creator
      xmp:CreateDate := 2020-10-10
      xmp:CreatorTool := A creator tool
      xmp:ModifyDate := 2023-01-27T13:37:27.909812682-08:00[America/Los_Angeles]

---

    Code
      print(x2)
    Output
      auto_xmp (not XMP tag):  photoshop:Credit
      dc:creator := A creator
      dc:description := A description
      dc:title := An XMP title
      pdf:Keywords := R, xmpdf
      pdf:Producer := R
      => photoshop:Credit = A creator
      xmp:CreateDate := 2020-10-10
      xmp:CreatorTool := A creator tool
      xmp:ModifyDate := 2023-01-27T13:37:27.909812682-08:00

