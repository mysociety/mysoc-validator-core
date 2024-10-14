from mysoc_validator_core._mysoc_validator_core import cli_convert
import sys

if __name__ == "__main__":

    args = sys.argv[1:]
    if len(args) == 0:
        print("No arguments provided. Please provide the path to the file you want to convert.")
    
    file_path = args[0]
    # if --verbose is provided, set verbose to True
    verbose = "--verbose" in args
    format_file = "--format" in args
    cli_convert(file_path, verbose, format_file)