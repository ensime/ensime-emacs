Feature: lsp mode
  This covers a few bases, it turns out

  Scenario: disallow connecting to swanky
    Given I got a config with version "3.0.0-SNAPSHOT" and source-root "/tmp"
    When I set temporary-file-directory to "/tmp/"
    And I open temp file "Hello.scala"
    Then the buffer is lsp only

  Scenario: allow connecting to swanky
    Given I got a config with version "2.0.0-SNAPSHOT" and source-root "/tmp"
    When I set temporary-file-directory to "/tmp/"
    And I open temp file "Hello.scala"
    Then the buffer is not lsp only
