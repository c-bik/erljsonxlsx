-module(erljsonxlsx).

-include_lib("kernel/include/file.hrl").

%% Shell APIs
-export([template_json/0, from_json/1, from_xlsx/1]).

%% ===================================================================
%% API functions
%% ===================================================================

-undef(SAMPLE).
-ifdef(SAMPLE).

{ok, Book1} = file:read_file("C:/projects/git/erljsonxlsx/samples/Book1.xlsx").
{ok, BookMap} = erljsonxlsx:from_xlsx(Book1).
io:format("~s~n", [jsx:prettify(jsx:encode(BookMap))]).

%zip:unzip(Book1, [memory]).

-endif.

-spec template_json() -> Json :: #{}.
template_json() ->
    #{name => <<"sample.xlsx">>,
      type => <<"zip">>,
      content =>
      [#{name => <<"[Content_Types].xml">>,
         type => <<"file">>,
         content => <<"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
                            "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">"
                                "<Default Extension=\"bin\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.printerSettings\"/>"
                                "<Default Extension=\"jpeg\" ContentType=\"image/jpeg\"/>"
                                "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>"
                                "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
                                "<Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/>"
                                "<Override PartName=\"/xl/worksheets/sheet1.xml\""
                                         " ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>"
                                "<Override PartName=\"/xl/worksheets/sheet2.xml\""
                                         " ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>"
                                "<Override PartName=\"/xl/worksheets/sheet3.xml\""
                                         " ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>"
                                "<Override PartName=\"/xl/theme/theme1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.theme+xml\"/>"
                                "<Override PartName=\"/xl/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml\"/>"
                                "<Override PartName=\"/xl/sharedStrings.xml\""
                                         " ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml\"/>"
                                "<Override PartName=\"/xl/drawings/drawing1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.drawing+xml\"/>"
                                "<Override PartName=\"/docProps/core.xml\" ContentType=\"application/vnd.openxmlformats-package.core-properties+xml\"/>"
                                "<Override PartName=\"/docProps/app.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.extended-properties+xml\"/>"
                            "</Types>">>
        },
       #{name => <<"_rels">>,
         type => <<"folder">>,
         content =>
         [#{name => <<".rels">>,
            type => <<"file">>,
            content =>
                <<"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
                  "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                       "<Relationship Id=\"rId3\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/"
                                                      "relationships/extended-properties\""
                                    " Target=\"docProps/app.xml\"/>"
                       "<Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/package/2006/"
                                                        "relationships/metadata/core-properties\""
                                    " Target=\"docProps/core.xml\"/>"
                       "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/"
                                                      "relationships/officeDocument\""
                                              " Target=\"xl/workbook.xml\"/>"
                  "</Relationships>">>
           }]
        },
       #{name => <<"docProps">>,
         type => <<"folder">>,
         content => []
        },
       #{name => <<"xl">>,
         type => <<"folder">>,
         content => []
        }
      ]
     }.

-spec from_json(Json :: #{}) -> {ok, #{}} | {error, term()}.
from_json(Json) when is_map(Json) ->
    {ok, #{}}.

-spec from_xlsx(XlsxBin :: binary()) -> {ok, Json :: #{}} | {error, term()}.
from_xlsx(XlsxBin) when is_binary(XlsxBin) ->
    ZipName = "",
    {ok, ZipInfo}
    = zip:foldl(
        fun(Name, GetInfo, GetBin, Acc) ->
                FileInfo = GetInfo(),
                FileContent
                = case FileInfo#file_info.type of
                      regular ->
                          FC = GetBin(),
                          case binary:match(FC,<<"<?xml ">>) of
                              nomatch -> FC;
                              _ -> xmljson:fromxml(FC)
                          end;
                      _ ->
                          GetBin()
                  end,
                [{filename:split(Name), FileInfo, FileContent} | Acc]
        end, [], {ZipName, XlsxBin}),
    {ok, build_map(ZipInfo, #{name => list_to_binary(ZipName),
                              type => <<"zip">>,
                              content => []})}.

build_map([], Map) -> Map;
build_map([Part|Parts], Map) ->
    build_map(Parts, build_map(Part, Map));
build_map({Name, #file_info{type = Type}, FileContent}, Map) ->
    add_path(Name, Type, FileContent, Map).

add_path([], _Type, _FileBin, Map) -> Map;
add_path([P], regular, FileContent, Map) ->
    maps:update(content, [#{name => list_to_binary(P), type => file,
                           content => FileContent} | maps:get(content, Map)], Map);
add_path([P], directory, _FileBin, Map) ->
    maps:update(content, [#{name => list_to_binary(P), type => directory,
                           content => []} | maps:get(content, Map)], Map);
add_path([P|Paths], Type, FileContent, Map) ->
    MapContents = maps:get(content, Map),
    {SubMap, NewMap}
    = case [M || M <- MapContents, maps:get(name, M) == P] of
          [Mp] -> {Mp, maps:update(content, MapContents -- [Mp], Map)};
          [] -> {#{name => list_to_binary(P), type => directory,
                   content => []}, Map}
      end,
    NewSubMap = add_path(Paths, Type, FileContent, SubMap),
    maps:update(content, [NewSubMap|MapContents], NewMap).
