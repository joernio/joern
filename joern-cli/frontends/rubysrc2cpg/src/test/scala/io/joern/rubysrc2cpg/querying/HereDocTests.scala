package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture

import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*

class HereDocTests extends RubyCode2CpgFixture(withDataFlow = true) {
  "HereDocs 1" should {
    val cpg = code("""
     |# frozen_string_literal: true
     |# By default this will return true, and thus all of the Capybara specs will
     |# fail until a developer using the site for training has patched up all of
     |# the vulnerabilities.
     |#
     |# However, RailsGoat maintainers need the Capybara features to pass to indicate
     |# changes to the site have not inadvertently removed or fixed any vulnerabilities
     |# since the whole point is to provide a site for a developer to fix.
     |$displayed_spec_notice = false
     |
     |def verifying_fixed?
     |  maintainer_env_name = "RAILSGOAT_MAINTAINER"
     |  result = !ENV[maintainer_env_name]
     |  if !$displayed_spec_notice && result
     |    puts <<-NOTICE
     |
     |    ******************************************************************************
     |    You are running the RailsGoat Capybara Specs in Training mode. These specs
     |    are supposed to fail, indicating vulnerabilities exist. They contain spoilers,
     |    so do not read the code in spec/vulnerabilities if your goal is to learn more
     |    about patching the vulnerabilities. You should fix the vulnerabilities in the
     |    application in order to get these specs to pass**. You can use them to measure
     |    your progress.
     |
     |    These same specs will pass if you set the #{maintainer_env_name} ENV variable.
     |
     |    **NOTE: The RSpec pending feature is used to toggle the outcome of these specs
     |    between Training mode and RailsGoat Maintainer mode. When the vulnerabilities
     |    are removed, the specs will pass instead. Try to get a fully passing suite.
     |    ******************************************************************************
     |
     |    NOTICE
     |    $displayed_spec_notice = true
     |  end
     |  result
     |end
     |
     |def login(user)
     |  visit "/"
     |  within(".signup") do
     |    fill_in "email", with: user.email
     |    fill_in "password", with: user.clear_password
     |  end
     |  within(".actions") do
     |    click_on "Login"
     |  end
     |end
     |
     |##Hack to fix PhantomJS errors on Mavericks - https://gist.github.com/ericboehs/7125105
     |module Capybara::Poltergeist
     |  class Client
     |    private
     |    def redirect_stdout
     |      prev = STDOUT.dup
     |      prev.autoclose = false
     |      $stdout = @write_io
     |      STDOUT.reopen(@write_io)
     |
     |      prev = STDERR.dup
     |      prev.autoclose = false
     |      $stderr = @write_io
     |      STDERR.reopen(@write_io)
     |      yield
     |    ensure
     |      STDOUT.reopen(prev)
     |      $stdout = STDOUT
     |      STDERR.reopen(prev)
     |      $stderr = STDERR
     |    end
     |  end
     |end
     |
     |class WarningSuppressor
     |  IGNORE_PATTERNS = [
     |    /QFont::setPixelSize: Pixel size <= 0/,
     |    /CoreText performance note:/,
     |    /WARNING: Method userSpaceScaleFactor/
     |  ]
     |
     |  def write(message)
     |    if ignore?(message)
     |      0
     |    else
     |      puts(message)
     |      1
     |    end
     |  end
     |
     |  private
     |
     |  def ignore?(message)
     |    IGNORE_PATTERNS.any? { |regexp| message =~ regexp }
     |  end
     |end
     |
     |Capybara.register_driver :poltergeist do |app|
     |  Capybara::Poltergeist::Driver.new(app, phantomjs_logger: WarningSuppressor.new, timeout: 60)
     |end
     |
     |Capybara.javascript_driver = :poltergeist
     """.stripMargin)

    "parse file" in {
      inside(cpg.method.name(":program").l) {
        case progMethod :: Nil => // Passing case
        case _                 => fail("Expected file to parse")
      }
    }
  }

  "HEREDOC 2 from Railsgoat" should {
    val cpg = code("""
        |# frozen_string_literal: true
        |require "spec_helper"
        |require "tmpdir"
        |
        |feature "csrf" do
        |  let(:normal_user) { UserFixture.normal_user }
        |
        |  before(:each) do
        |    UserFixture.reset_all_users
        |    pending unless verifying_fixed?
        |  end
        |
        |  scenario "attack\nTutorial: https://github.com/OWASP/railsgoat/wiki/R4-A8-CSRF", js: true do
        |    visit "/"
        |    # TODO: is there a way to get this without visiting root first?
        |    base_url = current_url
        |
        |    login(normal_user)
        |
        |    Dir.mktmpdir do |dir|
        |      hackety_file = File.join(dir, "form.on.bad.guy.site.html")
        |      post_url = "#{base_url}schedule.json"
        |      File.open(hackety_file, "w") do |f|
        |        f.print <<-HTML
        |        <html>
        |          <body>
        |            <form id='submit_me' action="#{post_url}" method="POST">
        |              <input type="hidden" name="schedule&#91;event&#95;name&#93;" value="Bad&#32;Guy" />
        |              <input type="hidden" name="schedule&#91;event&#95;type&#93;" value="pto" />
        |              <input type="hidden" name="schedule&#91;event&#95;desc&#93;" value="Fun&#32;Fun" />
        |              <input type="hidden" name="date&#95;range1" value="06&#47;08&#47;2013&#32;&#45;&#32;06&#47;09&#47;2013" />
        |              <input type="submit" value="Submit request" />
        |            </form>
        |          </body>
        |        </html>
        |        HTML
        |      end
        |
        |      page.driver.visit "file://#{hackety_file}"
        |      within("#submit_me") do
        |        click_on "Submit request"
        |      end
        |    end
        |
        |    expect(normal_user.reload.paid_time_off.schedule.last.event_name).not_to eq("Bad Guy")
        |  end
        |end
        |
        |""".stripMargin)

    "parse file with heredocs" in {
      inside(cpg.method.name(":program").l) {
        case programMethod :: Nil => // passing case
        case _                    => fail("Expected to find main program method")
      }
    }
  }

}
