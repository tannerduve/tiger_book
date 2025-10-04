import Lake
open Lake DSL

package «tiger_book» where
  -- Settings applied to both builds and interactive editing
  leanOptions := #[
    ⟨`pp.unicode.fun, true⟩ -- pretty-prints `fun a ↦ b`
  ]
  -- add any additional package configuration options here

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

require Parser from git
  "https://github.com/fgdorais/lean4-parser" @ "main"

require cslib from git
  "https://github.com/leanprover/cslib.git" @ "main"

@[default_target]
lean_lib «TigerBook» where
  -- add any library configuration options here
