-module(e9p_msg).

-include_lib("e9p/include/e9p.hrl").

-export([msg_type/1,
         tag/1,
         rest/1,
         fid/1,
         qid/1,
         afid/1,
         msize/1,
         stat/1,
         count/1,
         offset/1,
         data/1,
         aqid/1,
         name/1,
         uname/1,
         aname/1,
         ename/1,
         nwname/1,
         nwqid/1,
         newfid/1,
         iounit/1,
         perm/1,
         mode/1,
         qid_type/1,
         qid_version/1,
         qid_path/1
        ]).

-export([decode/1, decode_strict/1]).


%% Convenience macros for decoding common types
%%{{{

-define(uint8(Name),
        Name:8/unsigned-little-integer).
-define(uint16(Name),
        Name:16/unsigned-little-integer).
-define(uint32(Name),
        Name:32/unsigned-little-integer).
-define(uint64(Name),
        Name:64/unsigned-little-integer).

-define(pstring(SizeName, StringName),
        ?uint16(SizeName), StringName:SizeName/binary).

-define(tag(Name), ?uint16(Name)).
-define(fid(Name), ?uint32(Name)).
-define(qid_bytes(Name), Name:13/binary).

%%}}}


%%
%% Types and exports for messages
%%
%%{{{

-export_types([tag/0, fid/0, qid/0, dir/0, mode/0]).

-type tag() :: non_neg_integer().
%% Tags are 16-bit unsigned integers used to identify messages. Replies
%% (R-message) must have the same tag, identifying it as a response to that
%% T-message.
%%
%% The special value NOTAG is defined as `16#FFFF'.

-type fid() :: non_neg_integer().
%% A fid is a client-chosen integer identifying a file on the server.
%%
%% The special value NOFID is defined as `16#FFFFFFFF'.

-type qid() :: #qid{}.
%% A qid is a unique identifier for a file on the server.

-type qid_type() ::
        file
      | tmp
      | auth
      | mount
      | excl
      | append
      | dir.
%% The type of a qid.

-type dir() :: #dir{}.

-type mode() :: non_neg_integer().
%% An open mode. `truncate' may only be specified as the second element of a mode list.

-export_types([type/0, tmsg/0, rmsg/0, msg/0]).

-type msg_type() ::
        rversion
      | rauth
      | rerror
      | rflush
      | rattach
      | rwalk
      | ropen
      | rcreate
      | rread
      | rwrite
      | rclunk
      | rremove
      | rstat
      | rwstat
      | tversion
      | tauth
      | tflush
      | tattach
      | twalk
      | topen
      | tcreate
      | tread
      | twrite
      | tclunk
      | tremove
      | tstat
      | twstat.
%% type is any atom describing a T-message or R-message's type.

-type tmsg() ::
        tversion()
      | tauth()
      | tflush()
      | tattach()
      | twalk()
      | topen()
      | tcreate()
      | tread()
      | twrite()
      | tclunk()
      | tremove()
      | tstat()
      | twstat().
%% tmsg is any T-message.

-type rmsg() ::
        rversion()
      | rauth()
      | rerror()
      | rflush()
      | rattach()
      | rwalk()
      | ropen()
      | rcreate()
      | rread()
      | rwrite()
      | rclunk()
      | rremove()
      | rstat()
      | rwstat().
%% rmsg is any R-message.

-type msg() :: tmsg() | rmsg().
%% msg is any 9P message.

-export_types([
               tversion/0, rversion/0,
               tauth/0, rauth/0,
               rerror/0,
               tflush/0, rflush/0,
               tattach/0, rattach/0,
               twalk/0, rwalk/0,
               topen/0, ropen/0,
               tcreate/0, rcreate/0,
               tread/0, rread/0,
               twrite/0, rwrite/0,
               tclunk/0, rclunk/0,
               tremove/0, rremove/0,
               tstat/0, rstat/0,
               twstat/0, rwstat/0
              ]).

-type tversion() :: #tversion{}.
%% A Tversion message.
-type rversion() :: #rversion{}.
%% A Rversion message.
-type tauth() :: #tauth{}.
%% A Tauth message.
-type rauth() :: #rauth{}.
%% A Rauth message.
-type rerror() :: #rerror{}.
%% A Rerror message.
-type tflush() :: #tflush{}.
%% A Tflush message.
-type rflush() :: #rflush{}.
%% A Rflush message.
-type tattach() :: #tattach{}.
%% A Tattach message.
-type rattach() :: #rattach{}.
%% A Rattach message.
-type twalk() :: #twalk{}.
%% A Twalk message.
-type rwalk() :: #rwalk{}.
%% A Rwalk message.
-type topen() :: #topen{}.
%% A Topen message.
-type ropen() :: #ropen{}.
%% A Ropen message.
-type tcreate() :: #tcreate{}.
%% A Tcreate message.
-type rcreate() :: #rcreate{}.
%% A Rcreate message.
-type tread() :: #tread{}.
%% A Tread message.
-type rread() :: #rread{}.
%% A Rread message.
-type twrite() :: #twrite{}.
%% A Twrite message.
-type rwrite() :: #rwrite{}.
%% A Rwrite message.
-type tclunk() :: #tclunk{}.
%% A Tclunk message.
-type rclunk() :: #rclunk{}.
%% A Rclunk message.
-type tremove() :: #tremove{}.
%% A Tremove message.
-type rremove() :: #rremove{}.
%% A Rremove message.
-type tstat() :: #tstat{}.
%% A Tstat message.
-type rstat() :: #rstat{}.
%% A Rstat message.
-type twstat() :: #twstat{}.
%% A Twstat message.
-type rwstat() :: #rwstat{}.
%% A Rwstat message.

%%}}}

%%
%% Constructing messages
%%
%%{{{

%% file_mode([read|Opt]) -> truncate_mode(Opt);
%% file_mode([write|Opt]) -> 1 bor truncate_mode(Opt);
%% file_mode([readwrite|Opt]) -> 2 bor truncate_mode(Opt);
%% file_mode([exec|Opt]) -> 3 bor truncate_mode(Opt).

%% truncate_mode([]) -> 0;
%% truncate_mode([truncate]) -> 16#10.

%%%}}}


%%
%% Decode messages
%%
%%{{{

%% @doc Decode a 9P message. Upon success, returns a tuple of `ok', the message,
%% and the remaining un-decoded binary from `Bin'.
%%
%% If a message cannot be decoded from Bin, {error, {bad_msg, MsgType}} is
%% returned if the message is correctly typed. Otherwise, {error, badarg} is
%% returned for purely invalid binaries.
-spec decode(Bin :: binary())
            -> Result
                   when Result :: {ok, Decoded :: msg(), Rest :: binary()}
                                | {error, Reason},
                        Reason :: bad_type
                                | {bad_msg, MsgType :: msg_type()}.
decode(<<?uint32(Size), Rest0/binary>> = Bin) when byte_size(Bin) >= Size ->
    SuffixSize = Size - 5,
    <<?uint8(Type), Msg:SuffixSize/binary, Rest1/binary>> = Rest0,
    decode_with_type(decode_type(Type), Msg, Rest1);
decode(Bin) when is_binary(Bin) ->
    {error, badarg}.

%% @doc Decode a strict 9P message -- this does not permit tail data inside a 9P
%% message (i.e., an entire message's size must be for the 9P message).
%%
%% If a valid non-strict message is read, decode_strict will return
%% {error, {non_strict, MsgType}}. Otherwise, if the message is simply invalid,
%% it returns {error, {bad_msg, MsgType}}, the same as decode/1.
-spec decode_strict(Bin :: binary())
                   -> Result
                          when Result :: {ok, Decoded :: msg(), Rest :: binary()}
                                       | {error, Reason},
                               Reason :: bad_type
                                       | {bad_msg, MsgType}
                                       | {non_strict, MsgType},
                               MsgType :: msg_type().
decode_strict(Bin) when is_binary(Bin) ->
    decode_strict_validate(decode(Bin)).

decode_strict_validate({ok, Msg, _Rest} = Result) ->
    decode_strict_validate_rest(Result, rest(Msg));
decode_strict_validate({error, _Reason} = Error) ->
    Error.

decode_strict_validate_rest(Result, <<>>) ->
    Result;
decode_strict_validate_rest({ok, Msg, _Rest}, _MsgRest) ->
    {error, {non_strict, msg_type(Msg)}}.

%%
%% Internal
%%

decode_with_type({ok, Type}, Bin, Rest) ->
    decode_return_with_rest(decode_typed(Type, Bin), Rest);
decode_with_type({error, _} = Error, _Bin, _Rest) ->
    Error.

decode_return_with_rest({ok, Msg}, Rest) ->
    {ok, Msg, Rest};
decode_return_with_rest({error, _} = Error, _Rest) ->
    Error.

-spec decode_type(non_neg_integer()) -> msg_type() | {error, bad_type}.
decode_type(?Tversion) -> {ok, tversion};
decode_type(?Rversion) -> {ok, rversion};
decode_type(?Tauth)    -> {ok, tauth};
decode_type(?Rauth)    -> {ok, rauth};
decode_type(?Rerror)   -> {ok, rerror};
decode_type(?Tflush)   -> {ok, tflush};
decode_type(?Rflush)   -> {ok, rflush};
decode_type(?Tattach)  -> {ok, tattach};
decode_type(?Rattach)  -> {ok, rattach};
decode_type(?Twalk)    -> {ok, twalk};
decode_type(?Rwalk)    -> {ok, rwalk};
decode_type(?Topen)    -> {ok, topen};
decode_type(?Ropen)    -> {ok, ropen};
decode_type(?Tcreate)  -> {ok, tcreate};
decode_type(?Rcreate)  -> {ok, rcreate};
decode_type(?Tread)    -> {ok, tread};
decode_type(?Rread)    -> {ok, rread};
decode_type(?Twrite)   -> {ok, twrite};
decode_type(?Rwrite)   -> {ok, rwrite};
decode_type(?Tclunk)   -> {ok, tclunk};
decode_type(?Rclunk)   -> {ok, rclunk};
decode_type(?Tremove)  -> {ok, tremove};
decode_type(?Rremove)  -> {ok, rremove};
decode_type(?Tstat)    -> {ok, tstat};
decode_type(?Rstat)    -> {ok, rstat};
decode_type(?Twstat)   -> {ok, twstat};
decode_type(?Rwstat)   -> {ok, rwstat};
decode_type(_)         -> {error, bad_type}.

decode_typed(tversion, Msg) -> decode_tversion(Msg);
decode_typed(rversion, Msg) -> decode_rversion(Msg);
decode_typed(tauth, Msg)    -> decode_tauth(Msg);
decode_typed(rauth, Msg)    -> decode_rauth(Msg);
decode_typed(rerror, Msg)   -> decode_rerror(Msg);
decode_typed(tflush, Msg)   -> decode_tflush(Msg);
decode_typed(rflush, Msg)   -> decode_rflush(Msg);
decode_typed(tattach, Msg)  -> decode_tattach(Msg);
decode_typed(rattach, Msg)  -> decode_rattach(Msg);
decode_typed(twalk, Msg)    -> decode_twalk(Msg);
decode_typed(rwalk, Msg)    -> decode_rwalk(Msg);
decode_typed(topen, Msg)    -> decode_topen(Msg);
decode_typed(ropen, Msg)    -> decode_ropen(Msg);
decode_typed(tcreate, Msg)  -> decode_tcreate(Msg);
decode_typed(rcreate, Msg)  -> decode_rcreate(Msg);
decode_typed(tread, Msg)    -> decode_tread(Msg);
decode_typed(rread, Msg)    -> decode_rread(Msg);
decode_typed(twrite, Msg)   -> decode_twrite(Msg);
decode_typed(rwrite, Msg)   -> decode_rwrite(Msg);
decode_typed(tclunk, Msg)   -> decode_tclunk(Msg);
decode_typed(rclunk, Msg)   -> decode_rclunk(Msg);
decode_typed(tremove, Msg)  -> decode_tremove(Msg);
decode_typed(rremove, Msg)  -> decode_rremove(Msg);
decode_typed(tstat, Msg)    -> decode_tstat(Msg);
decode_typed(rstat, Msg)    -> decode_rstat(Msg);
decode_typed(twstat, Msg)   -> decode_twstat(Msg);
decode_typed(rwstat, Msg)   -> decode_rwstat(Msg).

%%
%% Per-type decode functions
%%
%%{{{

%% Qid

-define(QTFILE,   16#00).
-define(QTTMP,    16#04).
-define(QTAUTH,   16#08).
-define(QTMOUNT,  16#10).
-define(QTEXCL,   16#20).
-define(QTAPPEND, 16#40).
-define(QTDIR,    16#80).

-spec decode_qid_type(non_neg_integer()) -> qid_type() | {error, bad_qid}.
decode_qid_type(?QTFILE) ->
    {ok, file};
decode_qid_type(?QTTMP) ->
    {ok, tmp};
decode_qid_type(?QTAUTH) ->
    {ok, auth};
decode_qid_type(?QTMOUNT) ->
    {ok, mount};
decode_qid_type(?QTEXCL) ->
    {ok, excl};
decode_qid_type(?QTAPPEND) ->
    {ok, append};
decode_qid_type(?QTDIR) ->
    {ok, dir};
decode_qid_type(_Type) ->
    {error, bad_qid}.

decode_qid(<<?uint8(TypeBit), ?uint32(Version), ?uint64(Path)>>) ->
    decode_typed_qid(
      decode_qid_type(TypeBit),
      #qid{version = Version, path = Path});
decode_qid(_Qid) ->
    {error, bad_qid}.

decode_typed_qid({ok, Type}, Qid) ->
    {ok, Qid#qid{type = Type}};
decode_typed_qid({error, _} = Error, _Qid) ->
    Error.

%% Dir

decode_dir(<<?uint16(Size), Dir:Size/binary>>) ->
    decode_dir_fields(Dir);
decode_dir(_Dir) ->
    {error, bad_dir}.

decode_dir_fields(<<?uint16(Type),
                    ?uint32(Dev),
                    ?qid_bytes(QidBytes),
                    ?uint32(Mode),
                    ?uint32(Atime),
                    ?uint32(Mtime),
                    ?uint64(Length),
                    ?pstring(NameLen, Name),
                    ?pstring(UidLen, Uid),
                    ?pstring(GidLen, Gid),
                    ?pstring(MuidLen, Muid)
                  >>) ->
    decode_dir_qid(
      decode_qid(QidBytes),
      #dir{
         type   = Type,
         dev    = Dev,
         mode   = Mode,
         atime  = Atime,
         mtime  = Mtime,
         length = Length,
         name   = Name,
         uid    = Uid,
         gid    = Gid,
         muid   = Muid
        });
decode_dir_fields(_Dir) ->
    {error, bad_dir}.

decode_dir_qid({ok, Qid}, #dir{} = Dir) ->
    {ok, Dir#dir{qid = Qid}};
decode_dir_qid({error, _}, #dir{} = _Dir) ->
    {error, bad_dir}.

%% Version

-spec decode_tversion(binary()) -> {ok, tversion()} | {error, {bad_msg, tversion}}.
decode_tversion(<<?tag(Tag), ?uint32(Msize), ?pstring(Len, Version), Rest/binary>>) ->
    {ok,
     #tversion{
        tag     = Tag,
        msize   = Msize,
        version = Version,
        rest    = Rest
       }};
decode_tversion(_) ->
    {error, {bad_msg, tversion}}.

-spec decode_rversion(binary()) -> {ok, rversion()} | {error, {bad_msg, rversion}}.
decode_rversion(<<?tag(Tag), ?uint32(Msize), ?pstring(Len, Version), Rest/binary>>) ->
    {ok,
     #tversion{
        tag     = Tag,
        msize   = Msize,
        version = Version,
        rest    = Rest
       }};
decode_rversion(_Msg) ->
    {error, {bad_msg, rversion}}.

%% Auth

decode_tauth(<<?tag(Tag),
               ?uint32(Afid),
               ?pstring(UnameLen, Uname),
               ?pstring(AnameLen, Aname),
               Rest/binary
             >>) ->
    {ok,
     #tauth{
        tag   = Tag,
        afid  = Afid,
        uname = Uname,
        aname = Aname,
        rest  = Rest
       }};
decode_tauth(_Msg) ->
    {error, {bad_msg, tauth}}.

decode_rauth(<<?tag(Tag), ?qid_bytes(AqidBytes), Rest/binary>>) ->
    decode_rauth_aqid(
      decode_qid(AqidBytes),
      #rauth{
         tag  = Tag,
         rest = Rest
        });
decode_rauth(_Msg) ->
    {error, {bad_msg, rauth}}.

decode_rauth_aqid({ok, Qid}, #rauth{} = Rauth) ->
    {ok, Rauth#rauth{aqid = Qid}};
decode_rauth_aqid({error, _}, #rauth{} = _Rauth) ->
    {error, {bad_msg, rauth}}.

%% Error

decode_rerror(<<?tag(Tag), ?pstring(Len, Ename), Rest/binary>>) ->
    {ok,
     #rerror{
        tag   = Tag,
        ename = Ename,
        rest  = Rest
       }};
decode_rerror(_Msg) ->
    {error, {bad_msg, rerror}}.

%% Flush

decode_tflush(<<?tag(Tag), ?tag(OldTag), Rest/binary>>) ->
    {ok,
     #tflush{
        tag    = Tag,
        oldtag = OldTag,
        rest           = Rest
       }};
decode_tflush(_Msg) ->
    {error, {bad_msg, tflush}}.

decode_rflush(<<?tag(Tag), Rest/binary>>) ->
    {ok,
     #rflush{
        tag  = Tag,
        rest = Rest
       }};
decode_rflush(_Msg) ->
    {error, {bad_msg, rflush}}.

%% Attach

decode_tattach(<<?tag(Tag),
                 ?fid(Fid),
                 ?fid(Afid),
                 ?pstring(UnameLen, Uname),
                 ?pstring(AnameLen, Aname),
                 Rest/binary
               >>) ->
    {ok,
     #tattach{
        tag   = Tag,
        fid   = Fid,
        afid  = Afid,
        uname = Uname,
        aname = Aname,
        rest  = Rest
       }};
decode_tattach(_Msg) ->
    {error, {bad_msg, tattach}}.

decode_rattach(<<?tag(Tag), ?qid_bytes(QidBytes), Rest/binary>>) ->
    decode_rattach_qid(
      decode_qid(QidBytes),
      #rattach{
         tag  = Tag,
         rest = Rest
        });
decode_rattach(_Msg) ->
    {error, {bad_msg, rattach}}.

decode_rattach_qid({ok, Qid}, #rattach{} = Rattach) ->
    {ok, Rattach#rattach{qid = Qid}};
decode_rattach_qid({error, _}, #rattach{} = _Rattach) ->
    {error, {bad_msg, rattach}}.

%% Walk

decode_twalk(<<?tag(Tag), ?fid(Fid), ?fid(NewFid), ?uint16(NamesLen), Rest0/binary>>) ->
    decode_twalk_names(
      decode_twalk_name_list(Rest0, NamesLen, []),
      #twalk{
         tag    = Tag,
         fid    = Fid,
         newfid = NewFid
        });
decode_twalk(_Msg) ->
    {error, {bad_msg, twalk}}.

decode_twalk_names({ok, Names, Rest}, Twalk) ->
    {ok,
     Twalk#twalk{
       nwname = Names,
       rest   = Rest
      }};
decode_twalk_names({error, _}, #twalk{} = _Twalk) ->
    {error, {bad_msg, twalk}}.

decode_twalk_name_list(<<Rest/binary>>, 0, Names) ->
    {ok, lists:reverse(Names), Rest};
decode_twalk_name_list(<<?pstring(NameLen, Name), Rest/binary>>, N, Names) when N > 0 ->
    decode_twalk_name_list(Rest, N-1, [Name|Names]);
decode_twalk_name_list(_Msg, _N, _Names) ->
    {error, {bad_msg, twalk}}.

decode_rwalk(<<?tag(Tag), ?uint16(QidsLen), Rest0/binary>>) ->
    decode_rwalk_qids(
      decode_rwalk_qid_list(Rest0, QidsLen, []),
      #rwalk{tag = Tag});
decode_rwalk(_Msg) ->
    {error, {bad_msg, rwalk}}.

decode_rwalk_qids({ok, Qids, Rest}, #rwalk{} = Rwalk) ->
    {ok,
     Rwalk#rwalk{
       nwqid = Qids,
       rest  = Rest
      }};
decode_rwalk_qids({error, _}, #rwalk{} = _Rwalk) ->
    {error, {bad_msg, rwalk}}.

decode_rwalk_qid_list(<<Rest/binary>>, 0, Qids) ->
    {ok, lists:reverse(Qids), Rest};
decode_rwalk_qid_list(<<?qid_bytes(QidBytes), Rest/binary>>, N, Qids) when N > 0 ->
    decode_rwalk_qid_list({decode_qid(QidBytes), Rest}, N, Qids);
decode_rwalk_qid_list({{ok, Qid}, Rest}, N, Qids) when N > 0 ->
    decode_rwalk_qid_list(Rest, N-1, [Qid|Qids]);
decode_rwalk_qid_list(_Msg, _N, _Qids) ->
    {error, {bad_msg, rwalk}}.

%% Open

decode_topen(<<?tag(Tag), ?fid(Fid), ?uint8(Mode), Rest/binary>>) ->
    {ok,
     #topen{
        tag = Tag,
        fid = Fid,
        mode = Mode,
        rest = Rest
       }};
decode_topen(_Msg) ->
    {error, {bad_msg, topen}}.

decode_ropen(<<?tag(Tag), ?qid_bytes(QidBytes), ?uint32(IOUnit), Rest/binary>>) ->
    decode_ropen_qid(
      decode_qid(QidBytes),
      #ropen{
         tag = Tag,
         iounit = IOUnit,
         rest = Rest
        });
decode_ropen(_Msg) ->
    {error, {bad_msg, ropen}}.

decode_ropen_qid({ok, Qid}, #ropen{} = Ropen) ->
    {ok, Ropen#ropen{qid = Qid}};
decode_ropen_qid({error, _}, #ropen{} = _Ropen) ->
    {error, {bad_msg, ropen}}.



%% Create

decode_tcreate(<<?tag(Tag),
                 ?fid(Fid),
                 ?pstring(NameLen, Name),
                 ?uint32(Perm),
                 ?uint8(Mode),
                 Rest/binary
               >>) ->
    {ok,
     #tcreate{
        tag  = Tag,
        fid  = Fid,
        name = Name,
        perm = Perm,
        mode = Mode,
        rest = Rest
       }};
decode_tcreate(_Msg) ->
    {error, {bad_msg, tcreate}}.

decode_rcreate(<<?tag(Tag), ?qid_bytes(QidBytes), ?uint32(IOUnit), Rest/binary>>) ->
    decode_rcreate_qid(
      decode_qid(QidBytes),
      #rcreate{
         tag = Tag,
         iounit = IOUnit,
         rest = Rest
        });
decode_rcreate(_Msg) ->
    {error, {bad_msg, rcreate}}.

decode_rcreate_qid({ok, Qid}, #rcreate{} = Rcreate) ->
    {ok, Rcreate#rcreate{qid = Qid}};
decode_rcreate_qid({error, _}, #rcreate{} = _Rcreate) ->
    {error, {bad_msg, rcreate}}.

%% Read

decode_tread(<<?tag(Tag), ?fid(Fid), ?uint64(Offset), ?uint32(Count), Rest/binary>>) ->
    {ok,
     #tread{
        tag    = Tag,
        fid    = Fid,
        offset = Offset,
        count  = Count,
        rest   = Rest
       }};
decode_tread(_Msg) ->
    {error, {bad_msg, tread}}.

decode_rread(<<?tag(Tag), ?uint32(Count), Data:Count/binary, Rest/binary>>) ->
    {ok,
     #rread{
        tag  = Tag,
        data = Data,
        rest = Rest
       }};
decode_rread(_Msg) ->
    {error, {bad_msg, rread}}.

%% Write

decode_twrite(<<?tag(Tag),
                ?fid(Fid),
                ?uint64(Offset),
                ?uint32(Count),
                Data:Count/binary,
                Rest/binary
              >>) ->
    {ok,
     #twrite{
        tag    = Tag,
        fid    = Fid,
        offset = Offset,
        data   = Data,
        rest   = Rest
       }};
decode_twrite(_Msg) ->
    {error, {bad_msg, twrite}}.

decode_rwrite(<<?tag(Tag), ?uint32(Count), Rest/binary>>) ->
    {ok,
     #rwrite{
        tag   = Tag,
        count = Count,
        rest  = Rest
       }};
decode_rwrite(_Msg) ->
    {error, {bad_msg, rwrite}}.

%% Clunk

decode_tclunk(<<?tag(Tag), ?fid(Fid), Rest/binary>>) ->
    {ok,
     #tclunk{
        tag  = Tag,
        fid  = Fid,
        rest = Rest
       }};
decode_tclunk(_Msg) ->
    {error, {bad_msg, tclunk}}.

decode_rclunk(<<?tag(Tag), Rest/binary>>) ->
    {ok,
     #rclunk{
        tag  = Tag,
        rest = Rest
       }};
decode_rclunk(_Msg) ->
    {error, {bad_msg, rclunk}}.

%% Remove

decode_tremove(<<?tag(Tag), ?fid(Fid), Rest/binary>>) ->
    {ok,
     #tremove{
        tag  = Tag,
        fid  = Fid,
        rest = Rest
       }};
decode_tremove(_Msg) ->
    {error, {bad_msg, tremove}}.

decode_rremove(<<?tag(Tag), Rest/binary>>) ->
    {ok,
     #rremove{
        tag  = Tag,
        rest = Rest
       }};
decode_rremove(_Msg) ->
    {error, {bad_msg, rremove}}.

%% Stat

decode_tstat(<<?tag(Tag), ?fid(Fid), Rest/binary>>) ->
    {ok,
     #tstat{
        tag  = Tag,
        fid  = Fid,
        rest = Rest
       }};
decode_tstat(_Msg) ->
    {error, {bad_msg, tstat}}.

decode_rstat(<<?tag(Tag), ?uint16(DirLen), DirBytes:DirLen/binary, Rest/binary>>) ->
    decode_rstat_dir(
      decode_dir(DirBytes),
      #rstat{
         tag  = Tag,
         rest = Rest
        });
decode_rstat(_Msg) ->
    {error, {bad_msg, rstat}}.

decode_rstat_dir({ok, Dir}, #rstat{} = Rstat) ->
    {ok, Rstat#rstat{stat = Dir}};
decode_rstat_dir({error, _}, #rstat{} = _Rstat) ->
    {error, {bad_msg, rstat}}.

%% Wstat

decode_twstat(<<?tag(Tag), ?fid(Fid), ?uint16(DirLen), DirBytes:DirLen/binary, Rest/binary>>) ->
    decode_twstat_dir(
      decode_dir(DirBytes),
      #twstat{
         tag  = Tag,
         fid  = Fid,
         rest = Rest
        });
decode_twstat(_Msg) ->
    {error, {bad_msg, twstat}}.

decode_twstat_dir({ok, Dir}, #twstat{} = Twstat) ->
    {ok, Twstat#twstat{stat = Dir}};
decode_twstat_dir({error, _}, #twstat{} = _Twstat) ->
    {error, {bad_msg, twstat}}.

decode_rwstat(<<?tag(Tag), Rest/binary>>) ->
    {ok,
     #rwstat{
        tag  = Tag,
        rest = Rest
       }};
decode_rwstat(_Msg) ->
    {error, {bad_msg, rwstat}}.

%%}}} Per-type decode functions

%%}}} Decode messages


%%
%% Record accessors
%%
%%{{{

-spec msg_type(Msg :: msg()) -> msg_type().
msg_type(#tversion{}) -> tversion;
msg_type(#rversion{}) -> rversion;
msg_type(#tauth{})    -> tauth;
msg_type(#rauth{})    -> rauth;
msg_type(#rerror{})   -> rerror;
msg_type(#tflush{})   -> tflush;
msg_type(#rflush{})   -> rflush;
msg_type(#tattach{})  -> tattach;
msg_type(#rattach{})  -> rattach;
msg_type(#twalk{})    -> twalk;
msg_type(#rwalk{})    -> rwalk;
msg_type(#topen{})    -> topen;
msg_type(#ropen{})    -> ropen;
msg_type(#tcreate{})  -> tcreate;
msg_type(#rcreate{})  -> rcreate;
msg_type(#tread{})    -> tread;
msg_type(#rread{})    -> rread;
msg_type(#twrite{})   -> twrite;
msg_type(#rwrite{})   -> rwrite;
msg_type(#tclunk{})   -> tclunk;
msg_type(#rclunk{})   -> rclunk;
msg_type(#tremove{})  -> tremove;
msg_type(#rremove{})  -> rremove;
msg_type(#tstat{})    -> tstat;
msg_type(#rstat{})    -> rstat;
msg_type(#twstat{})   -> twstat;
msg_type(#rwstat{})   -> rwstat.

-spec tag(Msg :: msg()) -> tag().
tag(#tversion{tag = Tag}) -> Tag;
tag(#rversion{tag = Tag}) -> Tag;
tag(#tauth{tag = Tag})    -> Tag;
tag(#rauth{tag = Tag})    -> Tag;
tag(#rerror{tag = Tag})   -> Tag;
tag(#tflush{tag = Tag})   -> Tag;
tag(#rflush{tag = Tag})   -> Tag;
tag(#tattach{tag = Tag})  -> Tag;
tag(#rattach{tag = Tag})  -> Tag;
tag(#twalk{tag = Tag})    -> Tag;
tag(#rwalk{tag = Tag})    -> Tag;
tag(#topen{tag = Tag})    -> Tag;
tag(#ropen{tag = Tag})    -> Tag;
tag(#tcreate{tag = Tag})  -> Tag;
tag(#rcreate{tag = Tag})  -> Tag;
tag(#tread{tag = Tag})    -> Tag;
tag(#rread{tag = Tag})    -> Tag;
tag(#twrite{tag = Tag})   -> Tag;
tag(#rwrite{tag = Tag})   -> Tag;
tag(#tclunk{tag = Tag})   -> Tag;
tag(#rclunk{tag = Tag})   -> Tag;
tag(#tremove{tag = Tag})  -> Tag;
tag(#rremove{tag = Tag})  -> Tag;
tag(#tstat{tag = Tag})    -> Tag;
tag(#rstat{tag = Tag})    -> Tag;
tag(#twstat{tag = Tag})   -> Tag;
tag(#rwstat{tag = Tag})   -> Tag.

%% @doc Returns the tail of the Msg that was not parsed.
%% Non-empty tails are non-standard.
-spec rest(Msg :: msg()) -> binary().
rest(#tversion{rest = Rest}) -> Rest;
rest(#rversion{rest = Rest}) -> Rest;
rest(#tauth{rest = Rest})    -> Rest;
rest(#rauth{rest = Rest})    -> Rest;
rest(#rerror{rest = Rest})   -> Rest;
rest(#tflush{rest = Rest})   -> Rest;
rest(#rflush{rest = Rest})   -> Rest;
rest(#tattach{rest = Rest})  -> Rest;
rest(#rattach{rest = Rest})  -> Rest;
rest(#twalk{rest = Rest})    -> Rest;
rest(#rwalk{rest = Rest})    -> Rest;
rest(#topen{rest = Rest})    -> Rest;
rest(#ropen{rest = Rest})    -> Rest;
rest(#tcreate{rest = Rest})  -> Rest;
rest(#rcreate{rest = Rest})  -> Rest;
rest(#tread{rest = Rest})    -> Rest;
rest(#rread{rest = Rest})    -> Rest;
rest(#twrite{rest = Rest})   -> Rest;
rest(#rwrite{rest = Rest})   -> Rest;
rest(#tclunk{rest = Rest})   -> Rest;
rest(#rclunk{rest = Rest})   -> Rest;
rest(#tremove{rest = Rest})  -> Rest;
rest(#rremove{rest = Rest})  -> Rest;
rest(#tstat{rest = Rest})    -> Rest;
rest(#rstat{rest = Rest})    -> Rest;
rest(#twstat{rest = Rest})   -> Rest;
rest(#rwstat{rest = Rest})   -> Rest.


-spec fid(Msg) -> fid() when
      Msg :: tattach()
           | twalk()
           | topen()
           | tcreate()
           | tread()
           | twrite()
           | tclunk()
           | tremove()
           | tstat()
           | twstat().
fid(#tattach{fid = Fid}) -> Fid;
fid(#twalk{fid = Fid})   -> Fid;
fid(#topen{fid = Fid})   -> Fid;
fid(#tcreate{fid = Fid}) -> Fid;
fid(#tread{fid = Fid})   -> Fid;
fid(#twrite{fid = Fid})  -> Fid;
fid(#tclunk{fid = Fid})  -> Fid;
fid(#tremove{fid = Fid}) -> Fid;
fid(#tstat{fid = Fid})   -> Fid;
fid(#twstat{fid = Fid})  -> Fid.

-spec afid(Msg) -> fid() when
      Msg :: tauth()
           | tattach().
afid(#tauth{afid = Afid})   -> Afid;
afid(#tattach{afid = Afid}) -> Afid.

-spec msize(Msg) -> non_neg_integer() when
      Msg :: tversion()
           | rversion().
msize(#tversion{msize = Msize}) -> Msize;
msize(#rversion{msize = Msize}) -> Msize.

-spec qid(Msg) -> qid() when
      Msg :: rattach()
           | ropen()
           | rcreate()
           | rauth().
qid(#rattach{qid = Qid}) -> Qid;
qid(#ropen{qid = Qid})   -> Qid;
qid(#rcreate{qid = Qid}) -> Qid;
qid(#rauth{aqid = Qid}) -> Qid.

-spec stat(Msg) -> dir() when
      Msg :: twstat()
           | rstat().
stat(#twstat{stat = Stat}) -> Stat;
stat(#rstat{stat = Stat})  -> Stat.

-spec count(Msg) -> non_neg_integer() when
      Msg :: tread()
           | rwrite().
count(#tread{count = N})  -> N;
count(#rread{data = Data})  -> byte_size(Data);
count(#twrite{data = Data}) -> byte_size(Data);
count(#rwrite{count = N}) -> N.

-spec offset(Msg) -> non_neg_integer() when
      Msg :: tread()
           | twrite().
offset(#tread{offset = Offset})  -> Offset;
offset(#twrite{offset = Offset}) -> Offset.

-spec data(Msg) -> binary() when
      Msg :: rread()
           | twrite().
data(#rread{data = Data})  -> Data;
data(#twrite{data = Data}) -> Data.

-spec aqid(Msg :: rauth()) -> non_neg_integer().
aqid(#rauth{aqid = Aqid}) -> Aqid.

-spec name(Msg :: tcreate()) -> binary().
name(#tcreate{name = Name}) -> Name.

uname(#tauth{uname = Name})   -> Name;
uname(#tattach{uname = Name}) -> Name.

-spec aname(Msg) -> binary() when
      Msg :: tauth()
           | tattach().
aname(#tauth{aname = Name})   -> Name;
aname(#tattach{aname = Name}) -> Name.

-spec ename(Msg :: rerror()) -> binary().
ename(#rerror{ename = Name}) -> Name.

-spec nwname(Msg :: twalk()) -> [binary()].
nwname(#twalk{nwname = Names}) -> Names.

-spec nwqid(Msg :: rwalk()) -> [qid()].
nwqid(#rwalk{nwqid = Qids}) -> Qids.

-spec newfid(Msg :: twalk()) -> fid().
newfid(#twalk{newfid = Fid}) -> Fid.

-spec iounit(Msg) -> non_neg_integer() when
      Msg :: ropen() | rcreate().
iounit(#ropen{iounit = Unit})   -> Unit;
iounit(#rcreate{iounit = Unit}) -> Unit.

-spec perm(Msg :: tcreate()) -> non_neg_integer().
perm(#tcreate{perm = Perm}) -> Perm.

-spec mode(Msg) -> mode() when
      Msg :: topen() | tcreate().
mode(#topen{mode = Mode})   -> Mode;
mode(#tcreate{mode = Mode}) -> Mode.

-spec qid_type(Qid | Msg) -> qid_type() when
      Qid :: qid(),
      Msg :: rattach()
           | ropen()
           | rcreate()
           | rauth().
qid_type(#qid{type = Type}) -> Type;
qid_type(Msg)               -> qid_type(qid(Msg)).

-spec qid_version(Qid | Msg) -> non_neg_integer() when
      Qid :: qid(),
      Msg :: rattach()
           | ropen()
           | rcreate()
           | rauth().
qid_version(#qid{version = Version}) -> Version;
qid_version(Msg)                     -> qid_version(qid(Msg)).

-spec qid_path(Qid | Msg) -> non_neg_integer() when
      Qid :: qid(),
      Msg :: rattach()
           | ropen()
           | rcreate()
           | rauth().
qid_path(#qid{path = Path}) -> Path;
qid_path(Msg)               -> qid_path(qid(Msg)).

%%}}}
