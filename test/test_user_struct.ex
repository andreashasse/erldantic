defmodule TestUserStruct do
  @moduledoc """
  Test Elixir struct for erldantic struct handling.
  """

  defstruct [:name, :age, :email]

  @type t :: %__MODULE__{
    name: String.t(),
    age: non_neg_integer(),
    email: String.t()
  }
end