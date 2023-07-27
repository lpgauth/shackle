# This file is only used by Shackle as a dependency.
# Use rebar3 instead for compiling, running tests, etc.
defmodule Shackle.MixProject do
  use Mix.Project

  get_version = fn vsn ->
    case vsn do
      :git ->
        {tag, 0} = System.cmd("git", ["describe", "--tags", "--abbrev=0"])
        String.trim(tag) |> String.to_charlist()
      other ->
        other
    end
  end

  {:ok, [{:application, :shackle, props}]} = :file.consult("src/shackle.app.src")
  @props Keyword.take(props, [:applications, :description, :env, :mod, :vsn])
    |> Keyword.update!(:vsn, get_version)

  def application do
    @props
  end

  def project do
    [
      app: :shackle,
      version: to_string(application()[:vsn]),
      language: :erlang
    ]
  end
end
