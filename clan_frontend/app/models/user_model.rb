class UserModel
  def self.query_account_info(acc_id, token, lang)
    rpc = Rails.application.get_rpc
    rpc.call.ch_user_api.query_account_info(acc_id, token, lang)
  end

  def self.is_user_clan_admin(clan_info, user_id)
    m = clan_info['members'][user_id.to_s]
    return (not m.nil? and m['role'] == 'leader')
  end

  def self.get_username(user_id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_user_api.get_username(Integer(user_id)) || "User#{user_id}"
  end

  def self.wg_url(user_id)
    "http://worldoftanks.ru/community/accounts/#{user_id}/"
  end

  def self.test_acc_info(id)
    {}
  end
end
