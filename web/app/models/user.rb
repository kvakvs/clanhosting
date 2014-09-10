class User
  def self.is_user_clan_admin(clan_info, user_id)
    m = clan_info['members'][user_id.to_s]
    return (not m.nil? and m['role'] == 'leader')
  end
end
