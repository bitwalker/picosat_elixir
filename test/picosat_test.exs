defmodule PicosatTest do
  use ExUnit.Case
  doctest Picosat

  test "voting" do
    assert {:ok, [1, -2, 3]} = Picosat.solve([
      [1, 2, -3],
      [2, 3],
      [-2],
      [-1, 3]
    ])
  end

  test "unsatisfiable" do
    assert {:error, :unsatisfiable} = Picosat.solve([[-1], [1]])
  end
end
