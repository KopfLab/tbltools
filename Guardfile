# Ruby guard file (need ruby >2.2 and bundler installed: 'gem install bundler')
# To make sure all the gems are installed, run 'bundle install' once in terminal
# Then you can use the Makefile target 'make gui_dev' to start the GUI in development mode
# For browser livereload to work, need the browser extension: http://livereload.com/extensions/#installing-sections
# If the delay is too short for relaunching the app, increase the grace_period

port = 5000

guard 'process', name: 'Shiny', command: ['R', '-e', "devtools::load_all('.'); tbltools::tbl_run_peer_evaluation(readxl::read_excel(system.file(package = 'tbltools', 'extdata', 'roster_template.xlsx')), port = #{port})"] do
  watch(%r{NAMESPACE})
  watch(%r{R/.+\.R$})
end

guard 'livereload', grace_period: 3 do
  watch(%r{NAMESPACE})
  watch(%r{R/.+\.R$})
end
