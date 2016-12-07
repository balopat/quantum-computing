# Runs the command and prints a notification
def execute(cmd)
  if system(cmd)
    n 'Build succeeded', 'hspec', :success
  else
    n 'Build failed', 'hspec', :failed
  end
end

def run_all_tests
  execute %{
    stack clean; stack test
  }
end

guard :shell do
  watch(%r{.*\.cabal$})          { run_all_tests }
  watch(%r{test/Spec.hs$})       { run_all_tests }
  watch(%r{src/(.+)\.hs$})       { run_all_tests }
  watch(%r{test/(.+)Spec\.hs$})  { run_all_tests }
end
