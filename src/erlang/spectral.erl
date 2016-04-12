-module (spectral).
-export([start/1]).

start(Args) ->
  [SampleFolder | [StartIndex | [StopIndex | _]]] = Args,
  io:fwrite("SampleFolder: ~p!\n", [SampleFolder]),
  {Start, []} = string:to_integer(StartIndex),
  {Stop, []} = string:to_integer(StopIndex),
  SampleData = read_sample("/home/authentect/source/authentect_spectroscopic/output/sample.spr"),
  KnownDatas = read_known_datas("/home/authentect/source/authentect_spectroscopic/output", Start, Stop),
  BestMatch = find_best_match(SampleData, KnownDatas).

% Read in a sample spectroscopy file
read_sample(SampleFilename) ->
  {ok, Device} = file:open(SampleFilename, [read]),
  Data = dict:new(),
  try get_all_lines(Device, Data)
    after file:close(Device)
  end.

% read all the lines in the file.
get_all_lines(Device, SampleData) ->
  case get_data(Device, SampleData) of
    eof -> SampleData;
    Data -> get_all_lines(Device, Data)
  end.

% for each line in file get the data.
get_data(Device, SampleData) ->
  Line = io:get_line(Device, ""),
  case Line of
    eof -> eof;
    Line -> get_line_data(Line, SampleData)
  end.

% build a map of wavelength to absorb
get_line_data(Line, SampleData) ->
  [Wavelength | [Absorb | _]] = string:tokens(Line, " \n"),
  {AbsorbVal, []} = string:to_float(Absorb),
  D1 = dict:new(),
  ToMerge = dict:append(Wavelength, AbsorbVal, D1),
  dict:merge(fun(_, _, V2)->V2 end, ToMerge, SampleData).

read_known_datas(BaseFolder, StartIndex, StopIndex) ->
  Indicies = lists:seq(StartIndex, StopIndex),
  KnownDatas = dict:new(),
  lists:foldl(fun(Index, AccIn)->read_data(BaseFolder, Index, AccIn) end, KnownDatas, Indicies).

read_data(BaseFolder, Index, KnownDatas) ->
  SampleFilename = lists:concat([BaseFolder, "/sample_", Index, ".spr"]),
  SampleData = read_sample(SampleFilename),
  dict:append(SampleFilename, SampleData, KnownDatas).

find_best_match(SampleData, KnownDatas) ->
  MatchValues = dict:map(fun(SampleName, KnownData)->compare_data(SampleName, SampleData, KnownData) end, KnownDatas),
  MatchValues.
  %io:fwrite("MatchValues: ~p\n", [MatchValues]).

compare_data(SampleName, SampleData, KnownData) ->
  ComparisonSum = dict:fold(fun(K, V1, AccIn) -> compare_data_fold_fn(K, V1, AccIn, KnownData) end, 0, SampleData),
  io:fwrite("ComparisonSum: ~p, ~p\n", [SampleName, ComparisonSum]),
  ComparisonSum.
  % count up all the value compares and divide by # of values..

compare_data_fold_fn(Wavelength, Absorb1, Acc, KnownData) ->
  io:fwrite("Get data for wavelength: ~p\n", [Wavelength]),
  io:fwrite("KnownData: ~p\n", [KnownData]),
  KnownAbsorb = dict:fetch(Wavelength, KnownData),
  io:fwrite("KnownAbsorb: ~p\n", [KnownAbsorb]),
  Comparison = absorb_compare(Absorb1, KnownAbsorb),
  Acc + Comparison.

% Compare the absorption lines.
absorb_compare(A1, A2) ->
  io:fwrite("Comparing: ~p = ~p\n", [A1, A2]),
  if 
    A1>A2 ->
      A1 / A2;
    true ->
      A2 / A1
  end.
