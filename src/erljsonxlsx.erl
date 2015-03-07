-module(erljsonxlsx).

%% Shell APIs
-export([template_json/0, from_json/1, from_xlsx/1]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec template_json() -> Json :: #{}.
template_json() ->
    #{<<"root">> =>
      [#{<<"type">> => <<"folder">>,
         <<"name">> => <<"_rels">>,
         <<"content">> =>
         [#{<<"type">> => <<"file">>,
            <<"name">> => <<".rels">>,
            <<"content">> =>
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
       #{<<"type">> => <<"folder">>,
         <<"name">> => <<"docProps">>,
         <<"content">> => []
        },
       #{<<"type">> => <<"folder">>,
         <<"name">> => <<"xl">>,
         <<"content">> => []
        },
       #{<<"type">> => <<"file">>,
         <<"name">> => <<"[Content_Types].xml">>,
         <<"content">> => []
        }
      ]
     }.

-spec from_json(Json :: #{}) -> {ok, XlsxBin :: binary()} | {error, term()}.
from_json(Json) when is_map(Json) ->
    {ok, #{}}.

-spec from_xlsx(XlsxBin :: binary()) -> {ok, Json :: #{}} | {error, term()}.
from_xlsx(XlsxBin) when is_binary(XlsxBin) ->
    {ok, #{}}.
