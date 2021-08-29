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

  test "plain" do
    assert {:ok, [1]} = Picosat.solve([[1]])
    assert {:ok, [1, 2]} = Picosat.solve([[1, 2]])
    assert {:ok, [1, 2, 3]} = Picosat.solve([[1, 2, 3]])

    assert {:ok, [1, 2, 3, 4]} = Picosat.solve([[1, 2], [3, 4]])
    assert {:ok, [1, 2, 3, 4]} = Picosat.solve([[1], [2], [3], [4]])

    assert {:ok, [1, 2, 3, 4]} = Picosat.solve([[2], [1], [4, 3]])
  end

  test "negative" do
    assert {:ok, [-1]} = Picosat.solve([[-1]])
    assert {:ok, [-1, -2]} = Picosat.solve([[-1, -2]])
    assert {:ok, [-1, -2, -3]} = Picosat.solve([[-1, -2, -3]])
  end

  test "select_one" do
    assert {:ok, [1, -2, -3]} = Picosat.solve([[-1, -2, -3], [1]])
    assert {:ok, [-1, 2, -3]} = Picosat.solve([[-1, -2, -3], [2]])
    assert {:ok, [-1, -2, 3]} = Picosat.solve([[-1, -2, -3], [3]])
  end

  test "list_large" do
    check = fn num ->
      assert {:ok, r} = Picosat.solve((for n <- 1 .. (num - 1), do: [-n]) ++ [[num]])
      num = Enum.find(r, &(&1 == num))
      assert num
    end
    check.(49)
    check.(123)
  end

  test "sparse_list" do
    assert {:ok, [1, -2, -3, -4, 5]} = Picosat.solve([[1], [5]])
  end

  test "sparse_list_large" do
    check = fn num ->
      assert {:ok, r} = Picosat.solve([[1], [num]])
      num = Enum.find(r, &(&1 == num))
      assert num
    end
    check.(50)
    check.(5_000)
    # check.(2_147_483_647 + 1) # Fails
  end

  test "different_lengths" do
    assert {:ok, [1, -2, 3, -4, 5, -6]} = Picosat.solve([[1], [-2, -5, -6], [3, 5]])
  end

  test "unsatisfiable" do
    assert {:error, :unsatisfiable} = Picosat.solve([[-1], [1]])
    assert {:error, :unsatisfiable} = Picosat.solve([[-1, -2], []])
  end
end
