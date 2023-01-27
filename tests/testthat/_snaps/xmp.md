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
      dc:Creator := A Creator
      dc:Description := A description
      dc:Title := An XMP title
      pdf:Keywords := R, xmpdf
      pdf:Producer := R
      xmp:CreateDate := 2020-10-10
      xmp:CreatorTool := A creator tool
      xmp:ModifyDate := 2023-01-27T13:37:27.909812682-08:00[America/Los_Angeles]

---

    Code
      print(x2)
    Output
      dc:Creator := A Creator
      dc:Description := A description
      dc:Title := An XMP title
      pdf:Keywords := R, xmpdf
      pdf:Producer := R
      xmp:CreateDate := 2020-10-10
      xmp:CreatorTool := A creator tool
      xmp:ModifyDate := 2023-01-27T13:37:27.909812682-08:00
      x:XMPToolkit := Image::ExifTool 12.40

