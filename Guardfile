guard 'shell' do
  watch(%r{(src|test)/([^.].*?)(_tests)?.erl}) {|m| run_eunit(m[1], m[2]) }
end

# automatically run the eunit tests
def run_eunit(src, suite)
  cmd = "./rebar eunit skip_deps=true suite=#{suite}"
  puts "Executing #{cmd}"
  puts `#{cmd}`
  if $? == 0
    Growl.notify_ok "#{suite}: eunit passed.", title: "Guard: #{File.basename(Dir.pwd)}", icon: ".icons/success.png"
  else
    Growl.notify_error "#{suite}: eunit failed.", title: "Guard: #{File.basename(Dir.pwd)}", icon: ".icons/error.png"
  end
end
