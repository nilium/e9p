-module(e9p_msg_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("e9p/include/e9p.hrl").

%% The fields macro is used to define the fields function that returns a ziplist
%% of field names and indices. The names are used as accessors and the indices
%% used to extract the equivalent value from the record. This is only to ensure
%% that accessors return the value held by the record.
-define(fields(Name), fields(Name) -> lists:zip(record_info(fields, Name), lists:seq(2, record_info(size, Name)))).

?fields(tversion);
?fields(rversion).

%% Simple message construction

tag(Tag) -> i16(Tag).
fid(Fid) -> i32(Fid).
str(S) when is_binary(S) -> [i16(byte_size(S)), S];
str(S) when is_list(S) -> [i16(iolist_size(S)), S].
i16(N) -> <<N:16/unsigned-little-integer>>.
i32(N) -> <<N:32/unsigned-little-integer>>.
i64(N) -> <<N:64/unsigned-little-integer>>.

msg(Type, Fields) ->
    Data = [<<Type:8/unsigned-little-integer>>, Fields],
    Size = iolist_size(Data) + 4,
    iolist_to_binary([<<Size:32/unsigned-little-integer>>|Data]).

msg(Type, Fields, Tail) ->
    <<(msg(Type, Fields))/binary, Tail/binary>>.

%% Tests

%% {Desc, In, Out} is a simple test.
%% {Desc, In, Out, Subs} is a simple test accompanied by sub-tests to further
%% validate a result.
%%
%% Each sub-test decodes the input binary separately.
decode_nonstrict_test_fun({Desc, In, Out, Subs}) ->
    [decode_nonstrict_test_fun({Desc, In, Out})
     | [{SubDesc,
         fun() ->
                 Decoded = e9p_msg:decode(In),
                 SubTest(Decoded)
         end
        } || {SubDesc, SubTest} <- Subs]];
decode_nonstrict_test_fun({Desc, In, {ok, Rec, _} = Out}) ->
    {Desc,
     fun() ->
             {ok, Msg, _} = Got = e9p_msg:decode(In),
             ?assertEqual(Out, Got),
             [begin
                  ?assertEqual(e9p_msg:FieldName(Msg), element(Index, Msg))
              end
              || {FieldName, Index} <- fields(element(1, Rec))]
     end}.

decode_nonstrict_test_() ->
    Cases =
        [{"Decode a Tversion message",
          msg(?Tversion, [tag(16#0102), i32(16#03040506), str("9P2000"), <<"REST">>], <<"TAIL">>),
          {ok,
           #tversion{
              tag = 16#0102,
              msize = 16#03040506,
              version = <<"9P2000">>,
              rest = <<"REST">>
             }, <<"TAIL">>}},
         {"Decode a Rversion message",
          msg(?Rversion, [tag(16#0102), i32(16#03040506), str("9P2000"), <<"REST">>], <<"TAIL">>),
          {ok,
           #rversion{
              tag = 16#0102,
              msize = 16#03040506,
              version = <<"9P2000">>,
              rest = <<"REST">>
             }, <<"TAIL">>}}
        ],
    {inparallel,
     [decode_nonstrict_test_fun(Case) || Case <- Cases]}.
