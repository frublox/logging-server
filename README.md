# Logging Server

A personal logging server written for keeping logs of Cordova apps.

## Usage

`logging-server 12345 logfiles/` runs the logging server on port 12345 and writes the log files to the folder "logfiles" (it will be created if it doesn't exist already).

The server accepts HTTP POST requests containing either JSON or form-url-encoded logging data.

## Log Format

Logs are held as key-value pairs (usually sent in the form of JSON), labeled:

- appName
- message
- messageType (can be "Info" or "Error")
- location (can be coordinates in the form of "[latitude, longitude]"; anything else will be taken as an unknown location)
- uuid (a number)
- model
- platform

## To-do

- Better command-line interface for passing arguments
- Integration with some kind of SQL database
- A Windows Service for automatically managing the server
