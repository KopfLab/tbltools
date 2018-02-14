# Ruby guard file (need ruby >2.2 and bundler installed: 'gem install bundler')
# - to make sure all the gems are installed, run 'bundle install' once in terminal
# - to have a test app available for the guard, use tbl_setup_peer_evaluation to set one up
# - then use the Makefile target 'make gui_dev' to start the GUI in development mode
# For browser livereload to work, need the browser extension: http://livereload.com/extensions/#installing-sections
# If the delay is too short for relaunching the app, increase the grace_period

folder = 'peer_evaluation'
port = 5000

guard 'process', name: 'Shiny', command: ['R', '-e', "devtools::load_all('.'); tbltools::tbl_test_peer_evaluation('#{folder}', port = #{port})"] do
  watch(%r{^NAMESPACE$})
  watch(%r{R/.+\.R$})
  watch(%r{#{folder}/.+\.R$})
end

guard 'livereload', grace_period: 2 do
  watch(%r{^NAMESPACE$})
  watch(%r{R/.+\.R$})
  watch(%r{#{folder}/.+\.R$})
end
