# erljsonxlsx
XLSX to and from JSON

<hr>

# APIs

## Convert JSON into a XLSX file
```erljsonxlsx:template_json() -> Json :: #{}```
```erljsonxlsx:from_json(Json :: #{}) -> {ok, XlsxBin :: binary()} | {error, term()}```

## Make a XLSX from JSON
```erljsonxlsx:from_xlsx(XlsxBin :: binary()) -> {ok, Json :: #{}} | {error, term()}```
