# Dictionary provenance and license evidence

The dictionary bytes in this directory are third-party data, not covered
solely by emacs-kp's `COPYING`.  `MANIFEST.tsv` identifies the evidence for
every bundled file:

- `embedded` means the dictionary itself contains its copyright and license
  or redistribution notice.
- `README_hyph_*.txt` names the checked-in companion notice.
- `LICENSES.md` refers to one of the pinned upstream companion files below.

The pinned LibreOffice snapshot is
`8fb8e794237cff49ec212023f96bcdb7d3fbf56c`.  Immutable source links:

| Dictionaries | Upstream license evidence |
|---|---|
| `hyph_eo.dic` | [`eo/license-en.txt`](https://github.com/LibreOffice/dictionaries/blob/8fb8e794237cff49ec212023f96bcdb7d3fbf56c/eo/license-en.txt) |
| `hyph_id_ID.dic` | [`id/LICENSE-dict`](https://github.com/LibreOffice/dictionaries/blob/8fb8e794237cff49ec212023f96bcdb7d3fbf56c/id/LICENSE-dict) |
| `hyph_mr_IN.dic` | [`mr_IN/COPYING`](https://github.com/LibreOffice/dictionaries/blob/8fb8e794237cff49ec212023f96bcdb7d3fbf56c/mr_IN/COPYING) |
| `hyph_nl_NL.dic` | [`nl_NL/license_en_EN.txt`](https://github.com/LibreOffice/dictionaries/blob/8fb8e794237cff49ec212023f96bcdb7d3fbf56c/nl_NL/license_en_EN.txt) |
| `hyph_ru_RU.dic` | [`ru_RU/README_ru_RU.txt`](https://github.com/LibreOffice/dictionaries/blob/8fb8e794237cff49ec212023f96bcdb7d3fbf56c/ru_RU/README_ru_RU.txt) |

`hyph_eu.dic` is a legacy checked-in source whose byte is not present in the
pinned LibreOffice tree.  Its origin, copyright, redistribution terms, and
checksum are retained in `README_hyph_eu.txt` and `MANIFEST.tsv`; the updater
copies that verified local byte instead of pretending it came from the
snapshot.

`hyph_sa_IN.dic` is intentionally not bundled.  At the pinned snapshot it has
neither an in-file copyright/license notice nor a package companion that
states terms for the hyphenation data.  LibreOffice reverted a generic
`COPYING` addition because spelling and hyphenation files may have different
licenses.  Re-add Sanskrit only after an authoritative license statement for
that exact data is available.

License texts and notices remain the authority.  This index is provenance
metadata, not a reinterpretation of their terms.
