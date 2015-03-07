-module(erljsonxlsx).

%% Shell APIs
-export([template_json/0, from_json/1, from_xlsx/1]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec template_json() -> Json :: #{}.
template_json() -> #{}.

-spec from_json(Json :: #{}) -> {ok, XlsxBin :: binary()} | {error, term()}.
from_json(Json) when is_map(Json) ->
    {ok, #{}}.

-spec from_xlsx(XlsxBin :: binary()) -> {ok, Json :: #{}} | {error, term()}.
from_xlsx(XlsxBin) when is_binary(XlsxBin) ->
    {ok, #{}}.
