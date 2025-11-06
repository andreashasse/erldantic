defmodule TestUserStruct do
  @moduledoc """
  Test Elixir struct for spectra struct handling.
  """

  defstruct [:name, :age, :email]

  @type t :: %__MODULE__{
    name: String.t(),
    age: non_neg_integer(),
    email: String.t() | nil
  }
end
