%% e9p_msg.hrl may be included by client/server code that wants to do pattern
%% matching on messages. Otherwise, it's viable to simply use the accessor
%% functions in the e9p_msg modules.

-define(NOTAG, 16#FFFF).
-define(NOFID, 16#FFFFFFFF).

-record(qid, {
          type    = file :: e9p_msg:qid_type(),
          version = 0    :: non_neg_integer(),
          path    = 0    :: non_neg_integer()
         }).

%% Version

-define(Tversion, 100).
-define(Rversion, 101).

-record(tversion, {
          tag     = 0    :: e9p_msg:tag(),
          msize   = 0    :: non_neg_integer(),
          version = <<>> :: binary()
         }).

-record(rversion, {
          tag     = 0    :: e9p_msg:tag(),
          msize   = 0    :: non_neg_integer(),
          version = <<>> :: binary()
         }).

%% Auth

-define(Tauth, 102).
-define(Rauth, 103).

-record(tauth, {
          tag   = 0    :: e9p_msg:tag(),
          afid  = 0    :: e9p_msg:fid(),
          uname = <<>> :: binary(),
          aname = <<>> :: binary()
         }).

-record(rauth, {
          tag  = 0      :: e9p_msg:tag(),
          aqid = #qid{} :: e9p_msg:qid()
         }).

%% Error

-define(Rerror, 107).

-record(rerror, {
          tag   = 0    :: e9p_msg:tag(),
          ename = <<>> :: binary()
         }).

%% Flush

-define(Tflush, 108).
-define(Rflush, 109).

-record(tflush, {
          tag    = 0 :: e9p_msg:tag(),
          oldtag = 0 :: non_neg_integer()
         }).

-record(rflush, {
          tag = 0 :: e9p_msg:tag()
         }).

%% Attach

-define(Tattach, 104).
-define(Rattach, 105).

-record(tattach, {
          tag   = 0    :: e9p_msg:tag(),
          fid   = 0    :: e9p_msg:fid(),
          afid  = 0    :: e9p_msg:fid(),
          uname = <<>> :: binary(),
          aname = <<>> :: binary()
         }).

-record(rattach, {
          tag = 0      :: e9p_msg:tag(),
          qid = #qid{} :: e9p_msg:qid()
         }).

%% Walk

-define(Twalk, 110).
-define(Rwalk, 111).

-record(twalk, {
          tag    = 0  :: e9p_msg:tag(),
          fid    = 0  :: e9p_msg:fid(),
          newfid = 0  :: e9p_msg:fid(),
          nwname = [] :: [binary()]
         }).

-record(rwalk, {
          tag   = 0  :: e9p_msg:tag(),
          nwqid = [] :: [e9p_msg:qid()]
         }).

%% Open

-define(Topen, 112).
-define(Ropen, 113).

-record(topen, {
          tag  = 0      :: e9p_msg:tag(),
          fid  = 0      :: e9p_msg:fid(),
          %% The head of mode is always one of read, write, readwrite, or exec.
          %% The second element, if present, is always 'truncate'.
          %% mode cannot be more than two elements in length.
          mode = [read] :: [mode()]
         }).

-record(ropen, {
          tag    = 0      :: e9p_msg:tag(),
          qid    = #qid{} :: e9p_msg:qid(),
          iounit = 0      :: non_neg_integer()
         }).

%% Create

-define(Tcreate, 114).
-define(Rcreate, 115).

-record(tcreate, {
          tag  = 0      :: e9p_msg:tag(),
          fid  = 0      :: e9p_msg:fid(),
          name = <<>>   :: binary(),
          perm = 0      :: non_neg_integer(),
          mode = [read] :: [mode()]
         }).

-record(rcreate, {
          tag    = 0      :: e9p_msg:tag(),
          qid    = #qid{} :: e9p_msg:qid(),
          iounit = 0      :: non_neg_integer()
         }).

%% Read

-define(Tread, 116).
-define(Rread, 117).

-record(tread, {
          tag    = 0 :: e9p_msg:tag(),
          fid    = 0 :: e9p_msg:fid(),
          offset = 0 :: non_neg_integer(),
          count  = 0 :: non_neg_integer()
         }).

-record(rread, {
          tag   = 0    :: e9p_msg:tag(),
          count = 0    :: non_neg_integer(),
          data  = <<>> :: binary()
         }).

%% Write

-define(Twrite, 118).
-define(Rwrite, 119).

-record(twrite, {
          tag    = 0    :: e9p_msg:tag(),
          fid    = 0    :: e9p_msg:fid(),
          offset = 0    :: non_neg_integer(),
          count  = 0    :: non_neg_integer(),
          data   = <<>> :: binary()
         }).

-record(rwrite, {
          tag   = 0 :: e9p_msg:tag(),
          count = 0 :: non_neg_integer()
         }).

%% Clunk

-define(Tclunk, 120).
-define(Rclunk, 121).

-record(tclunk, {
          tag = 0 :: e9p_msg:tag(),
          fid = 0 :: e9p_msg:fid()
         }).

-record(rclunk, {
          tag = 0 :: e9p_msg:tag()
         }).

%% Remove

-define(Tremove, 122).
-define(Rremove, 123).

-record(tremove, {
          tag = 0 :: e9p_msg:tag(),
          fid = 0 :: e9p_msg:fid()
         }).

-record(rremove, {
          tag = 0 :: e9p_msg:tag()
         }).

%% Dir
%%
%% This isn't a message on its own, just a support record used by stat and wstat.

-record(dir, {
          type   = 0      :: non_neg_integer(),
          dev    = 0      :: non_neg_integer(),
          qid    = #qid{} :: e9p_msg:qid(),
          mode   = 0      :: non_neg_integer(),
          %% NOTE: atime and mtime suffer from the year 2038 bug. Extensions to
          %% the protocol will be necessary by the time that rolls around.
          %% If neither atime nor mtime are useful, consider using 0.
          atime  = 0      :: non_neg_integer(),
          mtime  = 0      :: non_neg_integer(),
          length = 0      :: non_neg_integer(),
          name   = <<>>   :: binary(),
          uid    = <<>>   :: binary(),
          gid    = <<>>   :: binary(),
          muid   = <<>>   :: binary()
         }).

%% Stat

-define(Tstat, 124).
-define(Rstat, 125).

-record(tstat, {
          tag = 0 :: e9p_msg:tag(),
          fid = 0 :: e9p_msg:fid()
         }).

-record(rstat, {
          tag  = 0  :: e9p_msg:tag(),
          stat = [] :: [e9p_msg:dir()]
         }).


%% Wstat

-define(Twstat, 126).
-define(Rwstat, 127).

-record(twstat, {
          tag  = 0  :: e9p_msg:tag(),
          fid  = 0  :: e9p_msg:fid(),
          stat = [] :: [e9p_msg:dir()]
         }).

-record(rwstat, {
          tag = 0 :: e9p_msg:tag()
         }).
