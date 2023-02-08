# get_xmp() / set_xmp()

    Code
      print(xmp())
    Output
      No XMP metadata found

---

    Code
      print(get_xmp(f)[[1]])
    Output
      No XMP metadata found

---

    Code
      print(x)
    Output
      i  spdx_id (not XMP tag) := CC-BY-4.0
      i  auto_xmp (not XMP tag) :=  cc:attributionName, cc:license, dc:rights, dc:subject,
              photoshop:Credit, xmpRights:Marked, xmpRights:UsageTerms,
              xmpRights:WebStatement
         Iptc4xmpCore:AltTextAccessibility := An alternative image text
         Iptc4xmpCore:ExtDescrAccessibility := An extended description (for accessibility) With newline
         Iptc4xmpCore:Location := A sublocation (legacy)
      => cc:attributionName = A creator
         cc:attributionURL := https://example.com/attribution
      => cc:license = https://creativecommons.org/licenses/by/4.0/
         cc:morePermissions := https://example.com/more-permissions
         dc:contributor := An updated contributor
         dc:creator := A creator
         dc:description := A description
      => dc:rights = © 2020 A creator. Some rights reserved.
      => dc:subject = R, xmpdf
         dc:title := An XMP title
         pdf:Keywords := R, xmpdf
         pdf:Producer := R
      => photoshop:Credit = A creator
         photoshop:DateCreated := 2020
         photoshop:Headline := A headline
         xmp:CreateDate := 2020-10-10
         xmp:CreatorTool := A creator tool
         xmp:ModifyDate := 2023-01-27T13:37:27.909812682-08:00[America/Los_Angeles]
      => xmpRights:Marked = TRUE
      => xmpRights:UsageTerms = This work is licensed to the public under the Creative Commons
              Attribution 4.0 International license
              https://creativecommons.org/licenses/by/4.0/
      => xmpRights:WebStatement = https://creativecommons.org/licenses/by/4.0/

---

    Code
      print(x, mode = "creative_commons")
    Output
      i  spdx_id (not XMP tag) := CC-BY-4.0
      i  auto_xmp (not XMP tag) :=  cc:attributionName, cc:license, dc:rights, dc:subject,
              photoshop:Credit, xmpRights:Marked, xmpRights:UsageTerms,
              xmpRights:WebStatement
      => cc:attributionName = A creator
         cc:attributionURL := https://example.com/attribution
      => cc:license = https://creativecommons.org/licenses/by/4.0/
         cc:morePermissions := https://example.com/more-permissions
      => dc:rights = © 2020 A creator. Some rights reserved.
      => xmpRights:Marked = TRUE
      => xmpRights:UsageTerms = This work is licensed to the public under the Creative Commons
              Attribution 4.0 International license
              https://creativecommons.org/licenses/by/4.0/
      => xmpRights:WebStatement = https://creativecommons.org/licenses/by/4.0/

---

    Code
      print(x, mode = "google_images")
    Output
      i  spdx_id (not XMP tag) := CC-BY-4.0
      i  auto_xmp (not XMP tag) :=  cc:attributionName, cc:license, dc:rights, dc:subject,
              photoshop:Credit, xmpRights:Marked, xmpRights:UsageTerms,
              xmpRights:WebStatement
         dc:creator := A creator
      => dc:rights = © 2020 A creator. Some rights reserved.
      => photoshop:Credit = A creator
      X  plus:Licensor (not currently supported by {xmpdf})
      => xmpRights:WebStatement = https://creativecommons.org/licenses/by/4.0/

---

    Code
      print(x, mode = "all")
    Output
      i  spdx_id (not XMP tag) := CC-BY-4.0
      i  auto_xmp (not XMP tag) :=  cc:attributionName, cc:license, dc:rights, dc:subject,
              photoshop:Credit, xmpRights:Marked, xmpRights:UsageTerms,
              xmpRights:WebStatement
         Iptc4xmpCore:AltTextAccessibility := An alternative image text
         Iptc4xmpCore:ExtDescrAccessibility := An extended description (for accessibility) With newline
         Iptc4xmpCore:Location := A sublocation (legacy)
      => cc:attributionName = A creator
         cc:attributionURL := https://example.com/attribution
      => cc:license = https://creativecommons.org/licenses/by/4.0/
         cc:morePermissions := https://example.com/more-permissions
         dc:contributor := An updated contributor
         dc:creator := A creator
         dc:description := A description
      => dc:rights = © 2020 A creator. Some rights reserved.
      => dc:subject = R, xmpdf
         dc:title := An XMP title
         pdf:Keywords := R, xmpdf
         pdf:Producer := R
      => photoshop:Credit = A creator
         photoshop:DateCreated := 2020
         photoshop:Headline := A headline
         xmp:CreateDate := 2020-10-10
         xmp:CreatorTool := A creator tool
         xmp:ModifyDate := 2023-01-27T13:37:27.909812682-08:00[America/Los_Angeles]
      => xmpRights:Marked = TRUE
      => xmpRights:UsageTerms = This work is licensed to the public under the Creative Commons
              Attribution 4.0 International license
              https://creativecommons.org/licenses/by/4.0/
      => xmpRights:WebStatement = https://creativecommons.org/licenses/by/4.0/

---

    Code
      print(x2)
    Output
         Iptc4xmpCore:AltTextAccessibility := An alternative image text
         Iptc4xmpCore:ExtDescrAccessibility := An extended description (for accessibility) With newline
         Iptc4xmpCore:Location := A sublocation (legacy)
         cc:attributionName := A creator
         cc:attributionURL := https://example.com/attribution
         cc:license := https://creativecommons.org/licenses/by/4.0/
         cc:morePermissions := https://example.com/more-permissions
         dc:contributor := An updated contributor
         dc:creator := A creator
         dc:description := A description
         dc:rights := © 2020 A creator. Some rights reserved.
         dc:subject := R, xmpdf
         dc:title := An XMP title
         pdf:Keywords := R, xmpdf
         pdf:Producer := R
         photoshop:Credit := A creator
         photoshop:DateCreated := 2020
         photoshop:Headline := A headline
         x:XMPToolkit := Image::ExifTool 12.40
         xmp:CreateDate := 2020-10-10
         xmp:CreatorTool := A creator tool
         xmp:ModifyDate := 2023-01-27T13:37:27.909812682-08:00
         xmpRights:Marked := TRUE
         xmpRights:UsageTerms := This work is licensed to the public under the Creative Commons
              Attribution 4.0 International license
              https://creativecommons.org/licenses/by/4.0/
         xmpRights:WebStatement := https://creativecommons.org/licenses/by/4.0/

