-record(job, {time, node, module, function, args, result}).
-record(code, {module, binary}).
-record(candidates, {job, nodes}).
-record(myJobs, {node, jobs}).

-define(JOB_BUCKET, <<"Jobs">>).
-define(MODULE_BUCKET, <<"Modules">>).
-define(CANDIDATE_BUCKET, <<"Candidates">>).
-define(MY_JOB_BUCKET, <<"MyJobs">>).

-define(READY, <<"ready">>).
-define(IMPORTANT, <<"important">>).
-define(RUNNING, <<"running">>).
-define(TERMINATED, <<"terminated">>).
-define(NODEDOWN, <<"nodedown">>).
