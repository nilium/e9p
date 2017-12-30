-module(e9p_msg).

-include("e9p_msg.hrl").

-export([msg_type/1,
         tag/1,
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

-export([decode/1]).


%% Convenience macros for decoding common types
%%{{{

-define(uint8(Name),
        Name:1/unsigned-little-integer).
-define(uint16(Name),
        Name:2/unsigned-little-integer).
-define(uint32(Name),
        Name:4/unsigned-little-integer).
-define(uint64(Name),
        Name:8/unsigned-little-integer).

-define(pstring(SizeName, StringName),
        SizeName:2/unsigned-little-integer, StringName:SizeName/binary).

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

-type mode() :: read
              | write
              | readwrite
              | exec
              | truncate.
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
%% Decode messages
%%

%% @doc Decode a 9P message. Upon success, returns a tuple of `ok', the message,
%% the remaining un-decoded binary from `Bin'.
-spec decode(Bin :: binary())
            -> Result
                   when Result :: {ok, Decoded :: msg(), Rest :: binary()}
                                | {error, Reason},
                        Reason :: bad_type
                                | {bad_msg, msg_type()}.
decode(<<?uint32(Size), Rest/binary>> = _Bin) ->
    SuffixSize = Size - 5,
    <<?uint8(Type), Msg:SuffixSize/binary, _/binary>> = Rest,
    case decode_type(Type) of
        {ok, TypeName} -> decode_typed(Msg, TypeName);
        {error, _} = Error -> Error
    end.

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

-spec decode_typed(tag(), binary())
                  -> {ok, msg()} | {error, term()}.
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

-spec decode_qid_type(non_neg_integer()) -> qid_type().
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
    {error, bad_type}.

decode_qid(<<?uint8(TypeBit), ?uint32(Version), ?uint64(Path)>>) ->
    case decode_qid_type(TypeBit) of
        {ok, Type} ->
            {ok, #qid{type = Type, version = Version, path = Path}};
        _ ->
            {error, bad_qid}
    end;
decode_qid(_Qid) ->
    {error, bad_qid}.

%% Version

-spec decode_tversion(binary()) -> {ok, tversion()} | {error, {bad_msg, tversion}}.
decode_tversion(<<?tag(Tag), ?uint32(Msize), ?pstring(Len, Version)>>) ->
    {ok,
     #tversion{
        tag     = Tag,
        msize   = Msize,
        version = Version
       }};
decode_tversion(_) ->
    {error, {bad_msg, tversion}}.

-spec decode_rversion(binary()) -> {ok, rversion()} | {error, {bad_msg, tversion}}.
decode_rversion(<<?tag(Tag), ?uint32(Msize), ?pstring(Len, Version)>>) ->
    {ok,
     #tversion{
        tag     = Tag,
        msize   = Msize,
        version = Version
       }};
decode_rversion(_Msg) ->
    {error, {bad_msg, rversion}}.

%% Auth

decode_tauth(<<?tag(Tag), ?uint32(Afid), ?pstring(UnameLen, Uname), ?pstring(AnameLen, Aname)>>) ->
    {ok,
     #tauth{
        tag   = Tag,
        afid  = Afid,
        uname = Uname,
        aname = Aname
       }};
decode_tauth(_Msg) ->
    {error, {bad_msg, tauth}}.

decode_rauth(<<?tag(Tag), ?qid_bytes(AqidBytes)>>) ->
    case decode_qid(AqidBytes) of
        {ok, Aqid} ->
            {ok,
             #rauth{
                tag  = Tag,
                aqid = Aqid
               }};
        {error, _} ->
            {error, {bad_msg, rauth}}
    end;
decode_rauth(_Msg) ->
    {error, {bad_msg, rauth}}.

%% Error

decode_rerror(<<?tag(Tag), ?pstring(Len, Ename)>>) ->
    {ok,
     #rerror{
        tag   = Tag,
        ename = Ename
       }};
decode_rerror(_Msg) ->
    {error, {bad_msg, rerror}}.

%% Flush

decode_tflush(Msg) ->
	{error, unimplemented}.

decode_rflush(Msg) ->
	{error, unimplemented}.

%% Attach

decode_tattach(Msg) ->
	{error, unimplemented}.

decode_rattach(Msg) ->
	{error, unimplemented}.

%% Walk

decode_twalk(Msg) ->
	{error, unimplemented}.

decode_rwalk(Msg) ->
	{error, unimplemented}.

%% Open

decode_topen(Msg) ->
	{error, unimplemented}.

decode_ropen(Msg) ->
	{error, unimplemented}.

%% Create

decode_tcreate(Msg) ->
	{error, unimplemented}.

decode_rcreate(Msg) ->
	{error, unimplemented}.

%% Read

decode_tread(Msg) ->
	{error, unimplemented}.

decode_rread(Msg) ->
	{error, unimplemented}.

%% Write

decode_twrite(Msg) ->
	{error, unimplemented}.

decode_rwrite(Msg) ->
	{error, unimplemented}.

%% Clunk

decode_tclunk(Msg) ->
	{error, unimplemented}.

decode_rclunk(Msg) ->
	{error, unimplemented}.

%% Remove

decode_tremove(Msg) ->
	{error, unimplemented}.

decode_rremove(Msg) ->
	{error, unimplemented}.

%% Stat

decode_tstat(Msg) ->
	{error, unimplemented}.

decode_rstat(Msg) ->
	{error, unimplemented}.

%% Wstat

decode_twstat(Msg) ->
	{error, unimplemented}.

decode_rwstat(Msg) ->
	{error, unimplemented}.



%%%}}} Per-type decode functions

file_mode([read|Opt]) -> truncate_mode(Opt);
file_mode([write|Opt]) -> 1 bor truncate_mode(Opt);
file_mode([readwrite|Opt]) -> 2 bor truncate_mode(Opt);
file_mode([exec|Opt]) -> 3 bor truncate_mode(Opt).

truncate_mode([]) -> 0;
truncate_mode([truncate]) -> 16#10.

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

-spec stat(Msg) -> [dir()] when
      Msg :: twstat()
           | rstat().
stat(#twstat{stat = Stat}) -> Stat;
stat(#rstat{stat = Stat})  -> Stat.

-spec count(Msg) -> non_neg_integer() when
      Msg :: tread()
           | rread()
           | twrite()
           | rwrite().
count(#tread{count = N})  -> N;
count(#rread{count = N})  -> N;
count(#twrite{count = N}) -> N;
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

-spec mode(Msg) -> [mode()] when
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
