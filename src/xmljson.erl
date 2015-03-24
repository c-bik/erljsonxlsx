-module(xmljson).

-include_lib("xmerl/include/xmerl.hrl").

-export([fromxml/1, toxml/1]).

-undef(SAMPLE).
-ifdef(SAMPLE).

%rr("c:/Program\ Files/erlang/erl6\.3/lib/xmerl\-1\.3\.7/include/xmerl.hrl").
Data = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>
<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"><Default Extension=\"bin\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.printerSettings\"/><Default Extension=\"jpeg\" ContentType=\"image/jpeg\"/><Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/><Default Extension=\"xml\" ContentType=\"application/xml\"/><Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/><Override PartName=\"/xl/worksheets/sheet1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/><Override PartName=\"/xl/worksheets/sheet2.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/><Override PartName=\"/xl/worksheets/sheet3.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/><Override PartName=\"/xl/theme/theme1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.theme+xml\"/><Override PartName=\"/xl/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml\"/><Override PartName=\"/xl/sharedStrings.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml\"/><Override PartName=\"/xl/drawings/drawing1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.drawing+xml\"/><Override PartName=\"/docProps/core.xml\" ContentType=\"application/vnd.openxmlformats-package.core-properties+xml\"/><Override PartName=\"/docProps/app.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.extended-properties+xml\"/></Types>".
{Root, []} = xmerl_scan:string(Data).
lists:flatten(xmerl:export_simple([Root],xmerl_xml)).

xmljson:fromxml(Data).

-endif.

fromxml(XmlData) when is_binary(XmlData) ->
    fromxml(binary_to_list(XmlData));
fromxml(XmlData) when is_list(XmlData) ->
    fromxml(xmerl_scan:string(XmlData));
fromxml({Xml, []}) -> maprec(Xml);
fromxml({_Xml, Rest}) -> exit({trailing_data, Rest}).

toxml(_JsonData) -> ok.

-define(ISREC(__R,__Type), is_record(__R,__Type)).
-define(RECINFO(__Type), record_info(fields,__Type)).

maprec({}) -> #{};
maprec([]) -> [];
maprec(yes) -> true;
maprec(no) -> false;
maprec(N) when is_number(N) -> N;
maprec([R|_] = RecList)
  when is_record(R,xmlAttribute); is_record(R,xmlText); is_record(R,xmlComment);
       is_record(R,xmlNsNode); is_record(R,xmlNamespace); is_record(R,xmlDecl);
       is_record(R,xmlPI); is_record(R,xmlElement); is_record(R,xmlDocument) ->
    lists:foldl(fun(Rec, Acc) -> [maprec(Rec)|Acc] end, [], RecList);
maprec([{Atm,Int}|_] = PropList) when is_atom(Atm), is_integer(Int) ->
    #{type => taglist,
      value => lists:foldl(fun({A,I}, M) -> maps:put(A,I,M) end, #{}, PropList)
     };
maprec([{_,V}]) when is_atom(V) -> maprec(V);
maprec(V) when is_list(V) -> list_to_binary(V);
maprec(V) when is_atom(V) -> #{type => atom, value => V};
maprec(R)
  when is_record(R,xmlAttribute); is_record(R,xmlText); is_record(R,xmlComment);
       is_record(R,xmlNsNode); is_record(R,xmlNamespace); is_record(R,xmlDecl);
       is_record(R,xmlPI); is_record(R,xmlElement); is_record(R,xmlDocument) ->
    lists:foldl(
      fun({Field, Value}, Map) ->
              maps:put(Field, maprec(Value),
                       case maps:is_key(type, Map) of
                           false -> maps:put(type, element(1,R), Map);
                           true -> Map
                       end)
      end, #{},
      lists:zip(
        case R of
            R when ?ISREC(R,xmlDecl)        -> ?RECINFO(xmlDecl);
            R when ?ISREC(R,xmlAttribute)   -> ?RECINFO(xmlAttribute);
            R when ?ISREC(R,xmlNamespace)   -> ?RECINFO(xmlNamespace);
            R when ?ISREC(R,xmlNamespace)   -> ?RECINFO(xmlNamespace);
            R when ?ISREC(R,xmlNsNode)      -> ?RECINFO(xmlNsNode);
            R when ?ISREC(R,xmlElement)     -> ?RECINFO(xmlElement);
            R when ?ISREC(R,xmlText)        -> ?RECINFO(xmlText);
            R when ?ISREC(R,xmlComment)     -> ?RECINFO(xmlComment);
            R when ?ISREC(R,xmlPI)          -> ?RECINFO(xmlPI);
            R when ?ISREC(R,xmlDocument)    -> ?RECINFO(xmlDocument)
        end, tl(tuple_to_list(R)))).
