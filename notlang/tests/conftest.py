from hypothesis import Settings
Settings.register_profile("default", Settings(max_examples=100))
Settings.register_profile("dev", Settings(max_examples=10))
