from hypothesis import settings
settings.register_profile("default", settings(max_examples=100))
settings.register_profile("dev", settings(max_examples=10))
