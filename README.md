# erljsonxlsx
XLSX to and from JSON

<hr>

# APIs

## Convert JSON into a XLSX file
```erlang
erljsonxlsx:template_json() -> Json :: #{}
erljsonxlsx:from_json(Json :: #{}) -> {ok, XlsxBin :: binary()} | {error, term()}
```

## Make a XLSX from JSON
```erlang
erljsonxlsx:from_xlsx(XlsxBin :: binary()) -> {ok, Json :: #{}} | {error, term()}
```

```erlang
|-- [Content_Types].xml
|-- _rels
|-- docProps
|   |-- app.xml
|   `-- core.xml
`-- xl
    |-- _rels
    |   `-- workbook.xml.rels
    |-- drawings
    |   |-- _rels
    |   |   `-- drawing1.xml.rels
    |   `-- drawing1.xml
    |-- media
    |   `-- image1.jpeg
    |-- printerSettings
    |   `-- printerSettings1.bin
    |-- sharedStrings.xml
    |-- styles.xml
    |-- theme
    |   `-- theme1.xml
    |-- workbook.xml
    `-- worksheets
        |-- _rels
        |   `-- sheet3.xml.rels
        |-- sheet1.xml
        |-- sheet2.xml
        `-- sheet3.xml
```
