defmodule Take5Test do
  use ExUnit.Case
  doctest Take5

  test "greets the world" do
    assert Take5.hello() == :world
  end
end
