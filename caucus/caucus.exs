region_manager = RegionManager.spawn

cand_registry = AbstractRegistry.create(CandStruct)

Candidate.spawn("Bernie", 50, 0, cand_registry)
Candidate.spawn("Biden", 25, 0, cand_registry)
Candidate.spawn("Tulsi", 10, 0, cand_registry)
Candidate.spawn("1", 10, 100000, cand_registry)
Candidate.spawn("2", 10, 0, cand_registry)
Candidate.spawn("3", 10, 0, cand_registry)
Candidate.spawn("4", 10, 0, cand_registry)
Candidate.spawn("5", 10, 0, cand_registry)
Candidate.spawn("6", 10, 0, cand_registry)


VoterRegistry.create(:a)

Voter.spawn("ABC", :a, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("DEF", :a, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("GHI", :a, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("JKL", :a, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("MNO", :a, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("PQR", :a, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("STU", :a, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("VWX", :a, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("YZ1", :a, cand_registry, StupidSort.generate("Biden"))
SleepyVoter.spawn("g", :a, cand_registry, StupidSort.generate("Biden"))
VoteLeader.spawn(:a, cand_registry, region_manager)

VoterRegistry.create(:b)

Voter.spawn("1", :b, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("2", :b, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("3", :b, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("4", :b, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("5", :b, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("6", :b, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("7", :b, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("8", :b, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("9", :b, cand_registry, StupidSort.generate("Biden"))
VoteLeader.spawn(:b, cand_registry, region_manager)

VoterRegistry.create(:c)

Voter.spawn("10", :c, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("11", :c, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("12", :c, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("13", :c, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("14", :c, cand_registry, StupidSort.generate("Bernie"))
VoteLeader.spawn(:c, cand_registry, region_manager)

VoterRegistry.create(:d)

Voter.spawn("14", :d, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("15", :d, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("16", :d, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("17", :d, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("18", :d, cand_registry, StupidSort.generate("Tulsi"))
VoteLeader.spawn(:d, cand_registry, region_manager)

VoterRegistry.create(:e)

Voter.spawn("19", :e, cand_registry, StupidSort.generate("6"))
Voter.spawn("20", :e, cand_registry, StupidSort.generate("6"))
Voter.spawn("21", :e, cand_registry, StupidSort.generate("6"))
Voter.spawn("22", :e, cand_registry, StupidSort.generate("6"))
Voter.spawn("23", :e, cand_registry, StupidSort.generate("6"))
VoteLeader.spawn(:e, cand_registry, region_manager)


ref = Process.monitor(region_manager)

receive do
  {:DOWN, ^ref, _, _, _} -> :ok
end

