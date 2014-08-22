require File.expand_path('../boot', __FILE__)

require 'rails/all'
require 'bertrpc'

# Require the gems listed in Gemfile, including any gems
# you've limited to :test, :development, or :production.
Bundler.require(*Rails.groups)

module Clanhosting
  RPC_CACHE_SIZE = 10

  class Application < Rails::Application
    # Settings in config/environments/* take precedence over those specified here.
    # Application configuration should go into files in config/initializers
    # -- all .rb files in that directory are automatically loaded.

    # Set Time.zone default to the specified zone and make Active Record auto-convert to this zone.
    # Run "rake -D time" for a list of tasks for finding time zone names. Default is UTC.
    # config.time_zone = 'Central Time (US & Canada)'

    # The default locale is :en and all translations from config/locales/*.rb,yml are auto loaded.
    # config.i18n.load_path += Dir[Rails.root.join('my', 'locales', '*.{rb,yml}').to_s]
    # config.i18n.default_locale = :de

    attr_accessor :bert_connection
    @bert_connection = nil

    def get_rpc
      return @bert_connection unless @bert_connection.nil?
      @bert_connection = BERTRPC::Service.new('localhost', 10000)
    end

    attr_accessor :clanhosting_ses
    @clanhosting_ses = nil
    def set_ses(s)
      @clanhosting_ses = s
    end

    def get_ses
      nil if session[:user_account].nil?
      if @clanhosting_ses.nil?
        @clanhosting_ses = get_rpc.call.get_session(session[:user_account])
      end
      @clanhosting_ses
    end
  end
end
