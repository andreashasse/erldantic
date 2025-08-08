#!/usr/bin/env -S uv run --quiet --script
# /// script
# dependencies = [
#   "openapi-spec-validator>=0.7.1",
# ]
# ///

"""
OpenAPI Schema Validator

This script validates OpenAPI 3.0 specifications using openapi-spec-validator.
Usage: ./validate_openapi.py <openapi_json_file>
"""

import sys
import json
from openapi_spec_validator import validate_spec


def validate_openapi_file(filepath):
    """Validate an OpenAPI specification file."""
    try:
        with open(filepath, 'r') as f:
            spec = json.load(f)
        
        # Validate the OpenAPI specification
        validate_spec(spec)
        print(f"✅ {filepath} is a valid OpenAPI 3.0 specification")
        return True
        
    except json.JSONDecodeError as e:
        print(f"❌ Invalid JSON in {filepath}: {e}")
        return False
    except Exception as e:
        print(f"❌ OpenAPI validation failed for {filepath}: {e}")
        return False


def main():
    if len(sys.argv) != 2:
        print("Usage: ./validate_openapi.py <openapi_json_file>")
        sys.exit(1)
    
    filepath = sys.argv[1]
    success = validate_openapi_file(filepath)
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()