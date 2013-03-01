-module(site_pt).

-export([parse_transform/2]).

parse_transform(F, Options) ->
 	OutFile = proplists:get_value(out_file, Options),
 	Module = filename:basename(OutFile,".erl"),
 	Forms=parse_trans_mod:rename_module(F, list_to_atom(Module)),
	parse_trans_pp:pp_src(Forms, OutFile),
    Forms.

