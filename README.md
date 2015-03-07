# erljsonxlsx
XLSX to and from JSON

<hr>

# APIs

## Convert JSON into a XLSX file
```erljsonxlsx:template_json() -> Json :: #{}```

```erljsonxlsx:from_json(Json :: #{}) -> {ok, , XlsxFile :: binary()} | {error, term()}```

## Make a XLSX from JSON
```erljsonxlsx:from_xlsx(XlsxFile :: file:filename()) -> {ok, Json :: #{}} | {error, term()}```
