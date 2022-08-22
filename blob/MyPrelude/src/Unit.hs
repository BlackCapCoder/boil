module Unit where


-- If A is a Category:
--   unit = id
--
-- If A is Pointed:
--   unit = point unit
--
-- If A is a Monoid:
--   unit = mempty
--
-- Otherwise, any arbitrary inhabitant of A

class Unit a where
  unit :: a


instance (Unit a1, Unit a2) => Unit (a1, a2) where
  unit = (,) unit unit

instance (Unit a1, Unit a2, Unit a3) => Unit (a1, a2, a3) where
  unit = (,,) unit unit unit

instance (Unit a1, Unit a2, Unit a3, Unit a4) => Unit (a1, a2, a3, a4) where
  unit = (,,,) unit unit unit unit

instance (Unit a1, Unit a2, Unit a3, Unit a4, Unit a5) => Unit (a1, a2, a3, a4, a5) where
  unit = (,,,,) unit unit unit unit unit

instance (Unit a1, Unit a2, Unit a3, Unit a4, Unit a5, Unit a6) => Unit (a1, a2, a3, a4, a5, a6) where
  unit = (,,,,,) unit unit unit unit unit unit

instance (Unit a1, Unit a2, Unit a3, Unit a4, Unit a5, Unit a6, Unit a7) => Unit (a1, a2, a3, a4, a5, a6, a7) where
  unit = (,,,,,,) unit unit unit unit unit unit unit

instance (Unit a1, Unit a2, Unit a3, Unit a4, Unit a5, Unit a6, Unit a7, Unit a8) => Unit (a1, a2, a3, a4, a5, a6, a7, a8) where
  unit = (,,,,,,,) unit unit unit unit unit unit unit unit

instance (Unit a1, Unit a2, Unit a3, Unit a4, Unit a5, Unit a6, Unit a7, Unit a8, Unit a9) => Unit (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  unit = (,,,,,,,,) unit unit unit unit unit unit unit unit unit

instance (Unit a1, Unit a2, Unit a3, Unit a4, Unit a5, Unit a6, Unit a7, Unit a8, Unit a9, Unit a10) => Unit (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  unit = (,,,,,,,,,) unit unit unit unit unit unit unit unit unit unit


