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
      auto_xmp (not XMP tag):  cc:attributionName, cc:license, photoshop:Credit, xmpRights:Marked, xmpRights:UsageTerms, xmpRights:WebStatement
      => cc:attributionName = A creator
      cc:attributionURL := https://example.com/attribution
      => cc:license = https://creativecommons.org/licenses/by/4.0/
      cc:morePermissions := https://example.com/more-permissions
      dc:creator := A creator
      dc:description := A description
      dc:title := An XMP title
      pdf:Keywords := R, xmpdf
      pdf:Producer := R
      => photoshop:Credit = A creator
      xmp:CreateDate := 2020-10-10
      xmp:CreatorTool := A creator tool
      xmp:ModifyDate := 2023-01-27T13:37:27.909812682-08:00[America/Los_Angeles]
      => xmpRights:Marked = TRUE
      => xmpRights:UsageTerms = This work is licensed to the public under the Creative Commons Attribution 4.0 International license https://creativecommons.org/licenses/by/4.0/
      => xmpRights:WebStatement = https://creativecommons.org/licenses/by/4.0/

---

    Code
      print(x2)
    Output
      auto_xmp (not XMP tag):  cc:attributionName, cc:license, photoshop:Credit, xmpRights:Marked, xmpRights:UsageTerms, xmpRights:WebStatement
      cc:attributionName := A creator
      cc:attributionURL := https://example.com/attribution
      cc:license := https://creativecommons.org/licenses/by/4.0/
      cc:morePermissions := https://example.com/more-permissions
      dc:creator := A creator
      dc:description := A description
      dc:title := An XMP title
      pdf:Keywords := R, xmpdf
      pdf:Producer := R
      photoshop:Credit := A creator
      xmp:CreateDate := 2020-10-10
      xmp:CreatorTool := A creator tool
      xmp:ModifyDate := 2023-01-27T13:37:27.909812682-08:00
      xmpRights:Marked := TRUE
      xmpRights:UsageTerms := This work is licensed to the public under the Creative Commons Attribution 4.0 International license https://creativecommons.org/licenses/by/4.0/
      xmpRights:WebStatement := https://creativecommons.org/licenses/by/4.0/

