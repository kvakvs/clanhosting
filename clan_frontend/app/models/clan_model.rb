class ClanModel
  # TODO: Request this from backend, cache on backend
  def self.get_members_helper(clan_info)
    # Assumes clan is cached in session
    clan_members = []
    clan_info['members'].each do |_, member|
      clan_members.append [member['account_name'], member['account_id']]
    end
    clan_members
  end

  def self.list_alliances(clan_id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_clan_api.list_alliances(clan_id)
  end

  def self.add_alliance(clan1, clan2)
    rpc = Rails.application.get_rpc
    rpc.call.ch_clan_api.add_alliance(clan1, clan2)
  end

  def self.break_alliance(clan1, clan2)
    rpc = Rails.application.get_rpc
    rpc.call.ch_clan_api.break_alliance(clan1, clan2)
  end

  def self.list_alliance_requests(clan_id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_clan_api.list_alliance_requests(clan_id)
  end

  def self.request_alliance(clan1, clan2)
    rpc = Rails.application.get_rpc
    rpc.call.ch_clan_api.request_alliance(clan1, clan2)
  end

  def self.delete_alliance_request(clan1, clan2)
    rpc = Rails.application.get_rpc
    rpc.call.ch_clan_api.delete_alliance_request(clan1, clan2)
  end

  def self.search_clans(query, lang)
    rpc = Rails.application.get_rpc
    clans = rpc.call.ch_clan_api.search_clans(query, lang)
    clans.map! { |clan|
      clan['abbreviation'] = clan['abbreviation'].force_encoding('utf-8')
      clan['name']         = clan['name'].force_encoding('utf-8')
      clan['motto']        = clan['motto'].force_encoding('utf-8')
      clan
    }
  end
end
