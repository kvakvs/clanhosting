class Clan
  # Assumes clan is cached in session
  def self.get_members_helper clan_info
    clan_members = []
    clan_info['members'].each do |_, member|
      clan_members.append [member['account_name'], member['account_id']]
    end
    clan_members
  end
end
