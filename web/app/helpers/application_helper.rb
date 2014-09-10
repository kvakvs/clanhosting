module ApplicationHelper
  def require_clan_admin
    return true if session[:is_clan_leader]
    require_acl('clan_admin')
  end

  def require_acl(acl_id)
    unless AccessRights.has_access(session[:user_clan], acl_id, session[:user_id])
      redirect_to root_path, :alert => t('acl.no_rights_to_see_this')
      false
    end
    true
  end
end
