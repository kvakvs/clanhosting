class SiteModel < ActiveRecord::Base
  def self.exists(clan)
    rpc = Rails.application.get_rpc
    1 == rpc.call.ch_site_api.exists(clan)
  end
end
