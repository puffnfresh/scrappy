package scrappy

import reflect.api.Universe

package object utils {
  def hasSyntheticFlag(u: Universe)(mods: u.Modifiers): Boolean = {
    val SYNTHETIC: u.FlagSet = (1l << 21).asInstanceOf[u.FlagSet]
    mods.hasFlag(SYNTHETIC)
  }
}
