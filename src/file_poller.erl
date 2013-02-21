% @author GG [http://aleph-nought.blogspot.com/]
% @copyright Whatever
% @doc <tt>file_poller</tt> provides a simple mechanism for monitoring changes to the contents of a single directory. 
% It defines a single function,
% <tt>init</tt>, which spawns an autonomous monitoring process and links it to
% the calling process. This process will then send a message to the calling 
% process every time it detects that the contents of the directory being
% monitored have changed. Usage:
% <pre>
% PollerPid = file_poller:init(&lt;directory&gt;,&lt;PCRE&gt;,&lt;polling interval&gt;).
% &lt;process incoming messages&gt;
% PollerPid ! ext.
% </pre>
%
% @TODO Monitor a list of directories rather than just a single directory.
% @TODO Implement error handling for common errors.

-module(file_poller).

% loop must be exported so that it's available to the spawn_link call in init,
% but should never be called directly.

-export([init/2, loop/4]).

% Include the definition of the file_info record.

-include_lib("kernel/include/file.hrl").

% @doc Spawn a poller process and returns its pid.
% Arguments are intepreted as follows:
% <ul>
% <li>Directory: The directory to be monitored. This can be relative or
% absolute; general sanity recommends the latter.</li>
% <li>Regex: A Perl-compatible regular expression.</li>
% <li>PollInterval: The interval at which the poller should poll the directory,
% in milliseconds.</li>
% </ul>
%
% Once spawned the process will send
%
% <pre>
% { newfile, [Filename] }
% </pre>
%
% to the calling process whenever files are added (or changed) in the monitored
% directory. This behavior continues until it receives <tt>exit</tt>, at which
% point the process terminates.
%
% @spec init(Directory::string(),Regex::string(),PollInterval::integer()) -> pid()

-define(PollInterval, 5000).

init(File,OwnerPid) ->
    spawn_link(
		file_poller, loop, [OwnerPid, File, ?PollInterval, 0]
	).

% @private
% Polling loop. Returns the list of files in Directory and 
% sends a tuple of the form 
%
%   { new_files, [ file list ]}
%
% every Time milliseconds to the specified pid containing the 
% list of files which match Regex.
%
% Send 'exit' to terminate process.
%
% Should never be called directly, use init instead.

loop(PID, File,PollInterval, LastPollTime) ->
	receive
		exit -> ok
	after
		PollInterval ->
			{MFlag, NewPollTime} = is_modified(File,LastPollTime),
			if
				MFlag =:= true ->
					PID ! { File, NewPollTime };
				true -> ok
			end,
			loop(PID, File, PollInterval, NewPollTime)
    end.

is_modified(Filename, LastPollTime) ->
    NewPollTime = calendar:datetime_to_gregorian_seconds(
        { date(), time() }
    ),
        {ok, FileInfo} = file:read_file_info(Filename),
	    IsModified = calendar:datetime_to_gregorian_seconds(
				FileInfo#file_info.mtime 
			) > LastPollTime,
		{IsModified,NewPollTime}.
       
