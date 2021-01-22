{suites, "", shot_SUITE}.

{config, ["test.config"]}.
{logdir, "ct_report"}.

{ct_hooks, [ct_shot_hook]}.
