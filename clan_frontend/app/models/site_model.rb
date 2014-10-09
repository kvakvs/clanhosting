class SiteModel < ActiveRecord::Base
  def self.exists(clan)
    rpc = Rails.application.get_rpc
    1 == rpc.call.ch_site_api.exists(clan)
  end

  def self.read(clan_id)
    rpc = Rails.application.get_rpc
    value = rpc.call.ch_site_api.read(clan_id)
    return nil unless value

    value['clan_id'] = clan_id
    # value['title'] = value['title'].force_encoding('utf-8')
    value
  end

  def self.update(clan_id, fields)
    rpc = Rails.application.get_rpc
    rpc.call.ch_site_api.update(clan_id, fields)
  end
end
