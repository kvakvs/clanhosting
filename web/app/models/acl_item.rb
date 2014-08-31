class AclItem
  ACLS = %w(clan_admin edit_forums moderate_forums post_news)

  def self.read_for_user(clan_id, user_id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_acl_api.read_for_user(clan_id, user_id)
  end

  def self.has_access(clan_id, acl, user_id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_acl_api.has_access(clan_id, acl, user_id)
  end

  # def self.delete_all(clan_id)
  #   rpc = Rails.application.get_rpc
  #   rpc.call.ch_acl_api.delete_all(clan_id)
  # end

  def self.grant(clan_id, acl, user_id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_acl_api.grant(clan_id, acl, user_id)
  end

  def self.revoke(clan_id, acl, user_id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_acl_api.revoke(clan_id, acl, user_id)
  end
end
