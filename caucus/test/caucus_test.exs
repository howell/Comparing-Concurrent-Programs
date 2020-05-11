defmodule CaucusTest do
  use ExUnit.Case
  doctest Caucus

  test "greets the world" do
    assert Caucus.hello() == :world
  end
end
