% -*- mode: erlang -*-

%% Compile included deps 
{"lib/*", [{i, "include"}, {outdir, "ebin"}, strict_record_tests]}.

%% Compile behaviour definitions first
{"src/gx_ui.erl", [{i, "include"}, {outdir, "ebin"}, strict_record_tests]}.

%% Compile main code in src and subfolders
{"src/*", [{i, "include"}, {outdir, "ebin"}, strict_record_tests]}.
{"src/*/*", [{i, "include"}, {outdir, "ebin"}, strict_record_tests]}.
