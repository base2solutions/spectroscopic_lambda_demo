-module (spectral).
-export([start/1]).

start(Args) ->
  [SampleFolder | [StartIndex | [StopIndex | _]]] = Args,
  io:fwrite("SampleFolder: ~p!\n", [SampleFolder]),
  {Start, []} = string:to_integer(StartIndex),
  {Stop, []} = string:to_integer(StopIndex),
  SampleData = read_sample(string:concat(SampleFolder, "/sample.spr")),
  KnownDatas = read_known_datas(SampleFolder, Start, Stop),
  BestMatch = find_best_match(SampleData, KnownDatas),
  io:fwrite("BestMatch: ~p\n", [BestMatch]).

% Read in a sample spectroscopy file
read_sample(SampleFilename) ->
  {ok, Device} = file:open(SampleFilename, [read]),
  Data = maps:new(),
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
  maps:put(Wavelength, AbsorbVal, SampleData).

read_known_datas(BaseFolder, StartIndex, StopIndex) ->
  Indicies = lists:seq(StartIndex, StopIndex),
  KnownDatas = maps:new(),
  lists:foldl(fun(Index, AccIn)->read_data(BaseFolder, Index, AccIn) end, KnownDatas, Indicies).

read_data(BaseFolder, Index, KnownDatas) ->
  SampleFilename = lists:concat([BaseFolder, "/sample_", Index, ".spr"]),
  SampleData = read_sample(SampleFilename),
  maps:put(SampleFilename, SampleData, KnownDatas).

find_best_match(SampleData, KnownDatas) ->
  maps:fold(fun(SampleName, KnownData, AccIn)->best_match_fold(SampleName, SampleData, KnownData, AccIn) end, {"", 0.0}, KnownDatas).

best_match_fold(SampleName, SampleData, KnownData, AccIn) ->
  {_, BestValue} = AccIn,
  SampleMatch = compare_data(SampleData, KnownData),
  if
    BestValue < SampleMatch ->
      {SampleName, SampleMatch};
    true ->
      AccIn
  end.

compare_data(SampleData, KnownData) ->
  ComparisonSum = maps:fold(fun(K, V1, AccIn) -> compare_data_fold_fn(K, V1, AccIn, KnownData) end, 0, SampleData),
  ComparisonSum / maps:size(SampleData).

compare_data_fold_fn(Wavelength, Absorb1, Acc, KnownData) ->
  KnownAbsorb = maps:get(Wavelength, KnownData, 0.0),
  Comparison = absorb_compare(Absorb1, KnownAbsorb),
  Acc + Comparison.

% Compare the absorption lines.
absorb_compare(A1, A2) ->
  if 
    A2 == A1 ->
      1.0;
    A2>A1 ->
      A1 / A2;
    true ->
      A2 / A1
  end.
